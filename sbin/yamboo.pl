#!/usr/bin/perl -w
#
# yamboo.pl
# Create source code tree for a particular PROJECT
# Usage:
# perl mergexml.pl -p="PROJECT"
# C. Hogan, 2008
# Version 1.8
#
#
# Status
# Works for: _DISTRO _PH
#

use Getopt::Long;
use File::Find;

&GetOptions("help"           => \$help,
            "p=s"          => \$selected_project);

#
# Define special flags and stuff.
# Script scans all text files; might be safer to scan only named extension files? (.F)
#
$manual_preprocess_files = " ";
$exclude_files = "yamboo.pl ";
@core_projects = ('yambo','ypp','p2y','a2y','f2y','e2y');
@user_projects = ('MAGNETIC','DISTRIBUTED','SC','RT','ELPH',
                  'YPP_ELPH','YPP_RT','YPP_SC','YPP_MAGNETIC',
                  'BOLTZMANN','YPP_BOLTZMANN','DEBUG','KERR');
$user_projects_string = join(" ",@user_projects); # Append a space
$files_to_skip = $manual_preprocess_files.$exclude_files;
#
# Exclude matches to #defines like _SC_KERNEL by adding a space or newline character
# CARE: Do not use @user_projects after this point.
#
@tmp1 = map { "_".$_." " } @user_projects;
@tmp2 = map { "_".$_."\n" } @user_projects;
@available_projects = (@tmp1,@tmp2);
#
# User interface
#
if($help or not $selected_project){ 
   print "\nSyntax: yamboo.pl -p=\"GPL\" \n   or   yamboo.pl -p=\"PROJECT\" \n  where PROJECT is one of: $user_projects_string\n" and exit;
}

#
# Check for exact match
#
$selected_project_found = 0;
foreach $project (@user_projects) {
   if($project eq $selected_project or $selected_project eq "GPL"){ $selected_project_found = 1};
}
if($selected_project_found){
  #
  # Open the log file for writing
  #
  open(LOG, ">yamboo.log") or die "Error opening log\n";
  &io("\nProcessing source for project $selected_project \n\n");
  &io("Protected files: $files_to_skip \n");
}
else {
  print "\nProject $selected_project not available. Select one of: $user_projects_string.\n" and exit;
}
#
# Special tag which flags GPL tags to be ignored.
#
$PROJECT_IGNORE_TAG = "$selected_project"."_IGNORE";
#
# Hard wired tags
#
$START_EXCLUDE_TAG = "GPL_EXCLUDE_START";
$END_EXCLUDE_TAG   = "GPL_EXCLUDE_END";
$START_INCLUDE_TAG = "GPL_INCLUDE_START";
$END_INCLUDE_TAG   = "GPL_INCLUDE_END";
$SVN_DIR = ".svn";
$GPL_OBJECTS_FILE = ".objects_gpl";
$OBJECTS_FILE = ".objects";
$GPL_FILE_SUFFIX = "_gpl";
#
# Get list of all directories, recursively
#
find sub {
   return unless -d $_ ;
   $directory_name = $File::Find::name; 
   if ($directory_name =~ m/$SVN_DIR/){ return; }  
   push (@directories_list, $directory_name."/");
}, ".";

#######################################################################
# Process each directory: main loop.                                  #
#######################################################################

#@directories_list = ();
#push(@directories_list,"src/ras_reels_mods/");
#push(@directories_list,"config/");
#push(@directories_list,"driver/");
#push(@directories_list,"ypp/");
#push(@directories_list,"src/pol_function/");


DIRECTORY_LOOP: foreach $directory_name (@directories_list) {
   &io("\n===== Processing directory $directory_name ===== \n");
   #
   # Get all filenames in directory, excluding subdirectories and binaries
   #
   @local_filenames = &get_filenames_in_directory($directory_name);

   #==================================================================#
   # GPL_OBJECT present: preprocess .objects_gpl file,                #
   # delete .F files not present in object list, rename as .objects   #
   #==================================================================#

   if (grep(/$GPL_OBJECTS_FILE/, @local_filenames) ){
      &io("... Found $GPL_OBJECTS_FILE file:\n");

      $file_name = $directory_name.$GPL_OBJECTS_FILE;
      open(GPL_OBJ_FILENAME, $file_name) 
         or die "Error opening $file_name\n";
      @gpl_objects_contents = <GPL_OBJ_FILENAME>;
      close(GPL_OBJ_FILENAME);
      #
      # Preprocess the .objects_gpl file.
      #
      @preprocessed_objects = @gpl_objects_contents;
      $found_project = &check_for_project_preprocess_flags(@gpl_objects_contents);
      if($found_project ge 0) {  # Dont preprocess if matches specific project
         @preprocessed_objects = &preprocessor(@gpl_objects_contents);
      }
      #
      # Dump into the .objects_gpl file again
      #
      open(GPL_OBJ_FILENAME, ">", $file_name) 
         or die "Error opening $file_name\n";
      print GPL_OBJ_FILENAME for @preprocessed_objects;
      close(GPL_OBJ_FILENAME);
      #
      # Get the filename list (include any files allowed by PROJECT)
      #
      @gpl_only_source_files = &get_source_from_object_list(@preprocessed_objects);
      # If no objects listed, flag the directory for deletion
      #
      if($#gpl_only_source_files+1 == 0) {
         push(@directories_for_deletion,$directory_name);
         push (@delete_list, $directory_name);
         &io("... Directory marked for deletion\n"); 
         next DIRECTORY_LOOP;
      }
      #
      # Delete the non GPL files
      #
      foreach $file_name (@local_filenames) {
         #
         # Only delete extra "xxx.F" files (save .objects, _gpl, .F.in files)
         #
         unless ($file_name =~ m/\.F$/) { next };  # at end of string = $
#        print "\n.... Processing file $file_name \n";
         $found = grep(/$file_name/, @gpl_only_source_files) ;
         unless ($found) { 
            &io("... Deleting non-GPL file: $file_name\n");
            unlink ($directory_name.$file_name); 
            push (@delete_list, $directory_name.$file_name);
         }
      }
      #
      # Move the .objects_gpl to .objects
      #
      &io("... Reverting GPL only $GPL_OBJECTS_FILE file\n");
      rename($directory_name.$GPL_OBJECTS_FILE,$directory_name.$OBJECTS_FILE);
      push (@delete_list, $directory_name.$GPL_OBJECTS_FILE);
   }

   @local_filenames = &get_filenames_in_directory($directory_name);
#  print LOG "REMAINING: ".join("\t",@local_filenames)."\n"; 

   #==================================================================#
   # Preprocess OTHER _gpl files                                      #
   #==================================================================#
   FILE_GPL: foreach $file_name (@local_filenames) { 
      if($file_name =~ m/$GPL_FILE_SUFFIX$/) {
         &io("... Found _gpl $file_name\n"); 
         $new_file_name = $file_name;
         $new_file_name =~ s/$GPL_FILE_SUFFIX//;
         #
         # Test if file without _gpl exists and contains $selected_project...???
         #
         open(SOURCE_FILE, $directory_name.$file_name) 
         or die "Error opening $directory_name.$file_name\n";
         @source_contents = <SOURCE_FILE>;
         close(SOURCE_FILE);

#        $proj_test = grep(/#(\w| )*defined $selected_project|# *ifdef $selected_project/,@source_contents);
#        $proj_test = grep(/#if defined $selected_project/,@source_contents);
#        if($proj_test){
#          print "... Removing GPL only $file_name file\n";
#          unlink ($directory_name.$file_name);
#          push (@delete_list, $directory_name.$file_name);
#        }
#        else{
           &io("... Reverting GPL only $file_name file\n");
           rename($directory_name.$file_name,$directory_name.$new_file_name);
           push (@delete_list, $directory_name.$file_name);
           next;
#        }
      }
   }

   @local_filenames = &get_filenames_in_directory($directory_name);
   #==================================================================#
   # Preprocess every file for #GPL_INCLUDE tags                      #
   #==================================================================#
   GPL_PREPROCESS: foreach $file_name (@local_filenames) { 

      if($exclude_files =~ m/$file_name/) {
            &io("... Skipping GPL preprocess of $file_name\n"); next GPL_PREPROCESS;
      }
#     print "... Processing individual file $file_name \n";
      #
      # Read contents of source file
      #
      open(SOURCE_FILE, $directory_name.$file_name) 
         or die "Error opening $directory_name.$file_name\n";
      @source_contents = <SOURCE_FILE>;
      close(SOURCE_FILE);
      #
      # Test if GPL TAGS are present in file
      #
      $gpl_test = grep(/$START_INCLUDE_TAG|$START_EXCLUDE_TAG/,@source_contents);
      if(not $gpl_test) { next }
      &io("... Processing GPL tagged file $file_name \n");
#     elsif($found_exc) {&io("... EXCLUDE tags\n")}
      #
      # Strip non GPL lines
      #
      open(MODIFIED_FILE, ">", $directory_name.$file_name."-stripped") 
         or die "Error opening $directory_name.$file_name -stripped\n";
      $exclude_status = 0; 
      $include_status = 0; 
      while($line = shift(@source_contents)) {
         #
         # Check that the GPL flags have not been overridden by PROJECT_IGNORE tags
         #
         if($line =~ m/$PROJECT_IGNORE_TAG/) { next }; # skip this line only
#        if($line =~ m/$PROJECT_IGNORE_TAG/ and 
#             ($line =~ m/[$START_EXCLUDE_TAG|$END_EXCLUDE_TAG]/)) { next }; # skip this line only
#        if($line =~ m/$PROJECT_IGNORE_TAG/ and 
#             ($line =~ m/[$START_INCLUDE_TAG|$END_INCLUDE_TAG]/)) { next }; # skip this line only
         #
         # Test to see if we are inside a GPL loop
         #
         if($line =~ m/$START_EXCLUDE_TAG/) { $exclude_status = 1; };
         if($line =~ m/$START_INCLUDE_TAG/) { $include_status = 1;  next };
         if($line =~ m/$END_INCLUDE_TAG/) { $include_status = 0; next};

# WRONG _ need to check that ! is first character in line
         if($include_status) { $line =~ s/\!// } # Strip first ! only
         if(not $exclude_status) {
            print MODIFIED_FILE "$line";
         }

         if($line =~ m/$END_EXCLUDE_TAG/) { $exclude_status = 0};
      } 
      close(MODIFIED_FILE);
      rename($directory_name.$file_name."-stripped",$directory_name.$file_name);

   }

   #==================================================================#
   # Preprocess every file for #define PROJECT tags                   #
   #==================================================================#
   @local_filenames = &get_filenames_in_directory($directory_name);
#  print "REMAINING: ".join("\t",@local_filenames)."\n"; 

   FILE_PREPROCESS: foreach $file_name (@local_filenames) { 
#     print "... Testing file $file_name \n";
      #
      # Skip the special or exclude files (already GPL processed?)
      #
      if("$exclude_files.$manual_preprocess_files.$OBJECTS_FILE" =~ m/$file_name/) {
            &io("... Skipping preprocess of $file_name\n"); next FILE_PREPROCESS;
      }
      #
      # Read contents of source file
      #
      open(SOURCE_FILE, $directory_name.$file_name) 
         or die "Error opening $directory_name.$file_name\n";
      @source_contents = <SOURCE_FILE>;
      close(SOURCE_FILE);
      #
      # Skip if no preprocessing needed at all
      #
      $found_project = &check_for_project_preprocess_flags(@source_contents);
      if($found_project == 0) { next FILE_PREPROCESS }; # No #define PROJECT present
      #
      # Open temporary file to hold the processed text, and fill it
      #
      open(MODIFIED_FILE, ">", $directory_name.$file_name."-stripped") 
         or die "Error opening $directory_name.$file_name -stripped\n";
      @preprocessed_source = ();
      @preprocessed_source = &preprocessor(@source_contents);
      print MODIFIED_FILE @preprocessed_source;
      close(MODIFIED_FILE);
      rename($directory_name.$file_name."-stripped",$directory_name.$file_name);
   }
} # directory loop
&io("\n yambo GPL created successfully.\n");
#
# Delete project only directories (check if SVN still works)
#
&io("\n Remaining user tasks:\n");
foreach $directory (@directories_for_deletion) {
   &io(" ---> Delete project directory $directory \n");
## rmdir($project_directory);
}
&io(" ---> Manually preprocess: $manual_preprocess_files \n");
#
# Create a batchfile for deleting svn entries for deleting files.
#
open(BATCH, ">", "svndelete.batch") or die "Error opening batch\n";
print BATCH (" svn delete --force ");
print BATCH join("\n svn delete --force ",@delete_list);
close(BATCH);
open(BATCH, ">", "delete.batch") or die "Error opening batch\n";
print BATCH (" rm -fr ");
print BATCH join("\n rm -fr ",@delete_list);
close(BATCH);

exit;
##########################################################
# END of program
##########################################################

##########################################################
sub convert_objects_to_source{
  my @objects = @_;
  my(@sources);
  $SOURCE_SUFFIX=".F";
  foreach $object (@objects){
    $object =~ s/\.o/$SOURCE_SUFFIX/; 
    push (@sources, $object); 
  }
  return @sources;
}

#---------------------------------------------------------------------#
# Scans a .object file for *.o files.
#---------------------------------------------------------------------#
sub get_source_from_object_list{
  my @file_contents = @_;
  my @objects = ();
  my(@line_elements,$element);
  foreach $line (@file_contents){
    @line_elements = split(/ /, $line );
    foreach $element (@line_elements){
       if ($element =~ m/\.o/){ 
         push (@objects, $element);
       }
    }
  }
  @gpl_only_source_files = &convert_objects_to_source(@objects);
# print join("\n",@gpl_only_source_files); 
  return @gpl_only_source_files;
}
#---------------------------------------------------------------------#
# This will get all the filenames in the directory 
# excluding subdirectories (e.g. .svn),
# and binary files (in case some are left over)
#---------------------------------------------------------------------#
sub get_filenames_in_directory{
  my $directory_name = shift(@_);
  my @RealNames = ();
  opendir(DIR, $directory_name) or die "Cannot opendir : $!";
  my @Names = readdir(DIR);
  foreach $name (@Names) {
#   if ($name =~ /^\.\.?$|$SVN_DIR/){ next; }
    if (not -f $directory_name.$name ){ next; }
    if (not -T $directory_name.$name ){ next; }
    push(@RealNames, $name); 
#   else { push(@RealNames, $name); }
  }
  closedir(DIR);
  return(@RealNames);
}
#---------------------------------------------------------------------#
# Copy report to screen and logfile
#---------------------------------------------------------------------#
sub io{
    my($io_string) = shift(@_);
    print $io_string;
    print LOG $io_string;
}
#---------------------------------------------------------------------#
# Checks the text passed via the list @_ for the presence
# of #define PROJECT statements.
# If no #defines are present at all, or there is no match with
# a user-defined project, then returns $match_project = 0.
#---------------------------------------------------------------------#
sub check_for_project_preprocess_flags{
    my @source_contents = @_;
    my $match_project = 0;
    my @match = grep(/\sdefined|ifdef|\!defined/,@source_contents);
    if($#match+1 == 0) { return $match_project };  # No #define present at all


    $match_project = grep(/$selected_project/,@match);  
    if($match_project gt 0) { 
       $match_project = -1;
       return $match_project };  # Matches to specific project

    foreach my $project (@available_projects) {
       $match_project += grep(/$project/,@match);  # only #define DOUBLE, etc
    }
    return $match_project;  # Matches any project (>0) or none (#define DOUBLE) (=0)
}
#---------------------------------------------------------------------#
# Act as a virtual preprocessor on the given @text.                   #
#---------------------------------------------------------------------#
sub preprocessor{
    my @unprocessed_text = @_;
    my $inclusive_loop = 0;
    my $exclusive_loop = 0;
    my $report_file_change = 0;
    my $preprocess_loop = 0;
    my $project_else = 0;
    my @preprocessed_lines = ();
    
    LINE: while($myline = shift(@unprocessed_text)) {
#      print "XXX $preprocess_loop $inclusive_loop $exclusive_loop $line";
       #
       # Test to see if we are inside a PROJECT loop
       #
       if($exclusive_loop and ($preprocess_loop == 0) and $myline =~ m/# *endif/ ) { $exclusive_loop = 0; next };
       if($inclusive_loop and ($preprocess_loop == 0) and $myline =~ m/# *endif/ ) { 
                                    $inclusive_loop = 0; $project_else = 0; next };
       if($preprocess_loop gt 0 and $myline =~ m/# *endif/ ) { $preprocess_loop += -1 };
       if($inclusive_loop and $myline =~ m/# *else/ ) { $project_else = 1; next };
       #
       # Turn on the inclusive stripping
       #
       DEFPROJ: foreach $project (@available_projects) {
#              print "$project ";

          if($myline =~ m/#(\w| )*defined|# *ifdef/ and $myline =~ m/$project/ ) { 
             # #(\w| )* means # followed by zero or more (*) characters (\w) or spaces
             #
             # If the line ALSO has a core project, leave it alone!
             #
             foreach $core_project (@core_projects) {
                if($myline =~ m/$core_project/ ) { last DEFPROJ };
             }
             #
             # If the line ALSO matches the selected project, leave it alone!
             #
             if($myline =~ m/$selected_project/ ) { last DEFPROJ };

             $inclusive_loop = 1;
             $report_file_change = 1;
             next LINE;
          }
          #
          # Turn on the exclusive stripping
          #
          if($myline =~ m/#(\w| )*!defined|# *ifndef/ and $myline =~ m/$project/ ) { 
              $exclusive_loop = 1; # 
              $report_file_change = 1;
              next LINE;
          }
       }

       if($myline =~ m/#(\w| )*defined|# *ifdef/ ) { $preprocess_loop += 1 }
       if($inclusive_loop and $project_else == 0) { next };

       push(@preprocessed_lines,$myline);

    } # end loop LINE
    if($report_file_change) {print "... Preprocessing file $file_name \n"}
    return @preprocessed_lines;
}
