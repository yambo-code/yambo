#!/usr/bin/perl -w
#
# Copyright (C) 2013 Conor Hogan
#
# Program: yamboo.pl
# Perl script to generate source code from trunk for a particular PROJECT
# Usage:
# yamboo.pl -p="PROJECT"
# Version:
# 1.9
#
# To-do list:
# Add dryrun
# Multiple projects
# Make a list of project tags to remove, might be easier to strip?
# Indicate if a file has been modified after preprocessing
# Search for insupported !if ndefined tags
#
# Status

use Getopt::Long;
use File::Find;
$dryrun = 0;
$verbose = 0;
$nodryrun = 1;

&GetOptions("help"          => \$help,
            "d"             => \$dryrun,
            "v"             => \$verbose,
            "p=s"           => \$input_projects_string);

# Check, if STDOUT is a terminal. If not, not ANSI sequences are
# # emitted.
 if(-t STDOUT) {
     $color_start{blue}="\033[34m";
     $color_end{blue}="\033[0m";
     $color_start{red}="\033[31m";
     $color_end{red}="\033[0m";
     $color_start{green}="\033[32m";
     $color_end{green}="\033[0m";
}
#

#
# ============= USER DEFINED FLAGS =====================
#
# List any source files here that do not get processed correctly by the script
$manual_preprocess_files = " ";
#
$exclude_files = "yamboo.pl";
$exclude_dirs = "sbin bin doc";
@core_projects = ('yambo','ypp','p2y','a2y','f2y','e2y');
@user_projects = ('ELPH', 'YPP_ELPH', 'KERR', 'SURF', 'YPP_SURF','PL',
                  'QED','MAGNETIC','SC','RT','YPP_RT','YPP_SC','YPP_MAGNETIC');
#
# ============= END OF USER DEFINED FLAGS  ==============
#
$user_projects_string = join(" ",@user_projects); # Append a space
$files_to_skip = $manual_preprocess_files.$exclude_files;
#
# CARE: Do not use @user_projects after this point.
#
@tmp1 = map { "_".$_." " } @user_projects;
@tmp2 = map { "_".$_."\n" } @user_projects;
@available_projects = (@tmp1,@tmp2);
#
# Excluded directories
#
@exclude_dirs_list = split(/ /,$exclude_dirs);
chomp(@exclude_dirs_list);
#
# User interface
#
if($help or not $input_projects_string){ 
   &usage;
   print "Allowed PROJECTs are: $user_projects_string\n" and exit;
#  print "\nSyntax: yamboo.pl -p=\"GPL\"  (GPL tag will overwrite everything else!)\n   OR   yamboo.pl -p=\"PROJECT1 PROJECT2 ... \" \n  where allowed PROJECTs are: $user_projects_string\n" and exit;
}
#
# Extract list of selected projects
#
@input_projects_list = split(/ /,$input_projects_string);
chomp(@input_projects_list);
#
# Find exact matches, make definitive list
#
$number_selected_projects = 0;
foreach $project (@user_projects) {
   foreach $input_project (@input_projects_list) {
      if($project eq $input_project) {
         push(@selected_projects_list,$project);
         $number_selected_projects += 1;
      }
   }
}
$selected_projects_string = join(" ",@selected_projects_list); # Append a space
#
# Report projects identified correctly or GPL only
#
if($number_selected_projects){
  open(LOG, ">yamboo.log") or die "Error opening log\n";
  &io("Processing source for GPL + $selected_projects_string ");
}
elsif(index($input_projects_string,"GPL") ge 0){
  open(LOG, ">yamboo.log") or die "Error opening log\n";
  &io("Processing source for GPL only ");
}
else{
  print "Incorrect project selection: $input_projects_string. \nSelect from: $user_projects_string\n" and exit;
}

#
# Protected files:
#
&io("\nProtected files      : $files_to_skip \n");
&io("\nProtected directories: $exclude_dirs \n");
#
# Dry run
#
if($dryrun) {
   $nodryrun = 0;
   &io("\nDRY RUN: files will not be modified. \n");
}
#
# Verbose
#
if($verbose) {
   &io("\nVERBOSE output selected. \n");
}

#
# Special tags which flags GPL tags to be ignored.
#
#$PROJECT_IGNORE_TAG = "$selected_project"."_IGNORE";
@PROJECT_IGNORE_TAGS = @selected_projects_list;
foreach (@PROJECT_IGNORE_TAGS) { $_ = $_."_IGNORE" } ;
if($verbose) { print "Ignore tags:".join(" ",@PROJECT_IGNORE_TAGS) };
#print @PROJECT_IGNORE_TAGS;
#
# Hard wired tags
#
$START_EXCLUDE_TAG = "GPL_EXCLUDE_START";
$END_EXCLUDE_TAG   = "GPL_EXCLUDE_END";
$START_INCLUDE_TAG = "GPL_INCLUDE_START";
$END_INCLUDE_TAG   = "GPL_INCLUDE_END";
$SVN_DIR = ".svn";
$GIT_DIR = ".git";
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
   if ($directory_name =~ m/$GIT_DIR/){ return; }  
   foreach my $dir (@exclude_dirs_list) {
      if ($directory_name =~ m/$dir/){ return; }  
   }
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
      &io("... Found GPL object file $GPL_OBJECTS_FILE ");

      $file_name = $directory_name.$GPL_OBJECTS_FILE;
      open(GPL_OBJ_FILENAME, $file_name) 
         or die "Error opening $file_name\n";
      @gpl_objects_contents = <GPL_OBJ_FILENAME>;
      close(GPL_OBJ_FILENAME);
      #
      # Preprocess the .objects_gpl file.
      #
# Output: $match_project
#         0 if no #defines are present at all
#         0 if no #define PROJECT are present at all
#        -1 if matches to any selected project
#        +n if matches to n projects
#     @preprocessed_objects = @gpl_objects_contents;
#      $found_project = &check_for_project_preprocess_flags(@gpl_objects_contents);
#     if($verbose) { print "... found $found_project matches to projects\n" };
#      if($found_project ge 0) {  # Preprocess if 
#         &io("... Preprocessing file $GPL_OBJECTS_FILE\n");
         @preprocessed_objects = &preprocessor(@gpl_objects_contents); 
#     }
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
      if($verbose) { print "... Allowed source files: @gpl_only_source_files\n" };
      # If no objects listed, flag the directory for deletion
      #
      if($#gpl_only_source_files+1 == 0) {
         push(@directories_for_deletion,$directory_name);
         push (@delete_list, $directory_name);
         &io("\n... Directory marked for deletion"); 
         next DIRECTORY_LOOP;
      }
      #
      # Delete the non GPL files
      #
      foreach $file_name (@local_filenames) {
         #
         # Only delete extra "xxx.F" files (save .objects, _gpl, .F.in files)
         #
         if($verbose) { print "\n.... Processing file $file_name \n" };
         unless ($file_name =~ m/\.F$/) { next };  # at end of string = $
         $found = grep(/$file_name/, @gpl_only_source_files) ;
         unless ($found) { 
            &io("\n... Deleting non-GPL file: $file_name");
            if($nodryrun) { unlink ($directory_name.$file_name) }; 
            push (@delete_list, $directory_name.$file_name);
         }
      }
      #
      # Move the .objects_gpl to .objects
      #
      &io("\n... Overwriting $OBJECTS_FILE with $GPL_OBJECTS_FILE");
      if($nodryrun) { rename($directory_name.$GPL_OBJECTS_FILE,$directory_name.$OBJECTS_FILE)};
      push (@delete_list, $directory_name.$GPL_OBJECTS_FILE);
   }

   @local_filenames = &get_filenames_in_directory($directory_name);
   if($verbose) {print LOG "REMAINING: ".join("\t",@local_filenames)."\n"}; 



   #==================================================================#
   # Preprocess OTHER _gpl files (e.g. Makefile.in_gpl, yambo.h_gpl   #
   #==================================================================#
   FILE_GPL: foreach $file_name (@local_filenames) { 
      if($file_name =~ m/$GPL_FILE_SUFFIX$/) {
         &io("\n... Found generic GPL suffix $GPL_FILE_SUFFIX file: $file_name"); 
         $new_file_name = $file_name;
         $new_file_name =~ s/$GPL_FILE_SUFFIX//;  # strip the suffix
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
           &io("\n... Reverting GPL only $file_name file");
           &io("\n... Overwriting $file_name with $new_file_name");
           if($nodryrun) { rename($directory_name.$file_name,$directory_name.$new_file_name) };
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
            &io("\n... Skipping GPL preprocess of $file_name\n"); next GPL_PREPROCESS;
      }
      if($verbose) {print "... Processing individual file $file_name \n"};
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
      &io("\n... Preprocessing GPL tagged file $file_name ");
#     elsif($found_exc) {&io("... EXCLUDE tags\n")}
      #
      # Strip non GPL lines
      #
      open(MODIFIED_FILE, ">", $directory_name.$file_name."-stripped") 
         or die "Error opening $directory_name.$file_name -stripped\n";
      $exclude_status = 0; 
      $include_status = 0; 
      LINE: while($line = shift(@source_contents)) {
         if($verbose) { print LOG substr($line,0,10)." incl=".$include_status." excl=".$exclude_status."\n" };
         #
         # Check that the GPL flags have not been overridden by PROJECT_IGNORE tags 
         #
         foreach my $PROJECT_IGNORE_TAG (@PROJECT_IGNORE_TAGS){
            if($line =~ m/$PROJECT_IGNORE_TAG/) { next LINE }; # skip this line only
         }
#        if($line =~ m/$PROJECT_IGNORE_TAG/ and 
#             ($line =~ m/[$START_EXCLUDE_TAG|$END_EXCLUDE_TAG]/)) { next }; # skip this line only
#        if($line =~ m/$PROJECT_IGNORE_TAG/ and 
#             ($line =~ m/[$START_INCLUDE_TAG|$END_INCLUDE_TAG]/)) { next }; # skip this line only
         #
         # Test to see if we are inside a GPL loop
         #
         if($line =~ m/$START_EXCLUDE_TAG/) { $exclude_status = 1; };
         if($line =~ m/$START_INCLUDE_TAG/) { $include_status = 1;  next LINE };
         if($line =~ m/$END_INCLUDE_TAG/) { $include_status = 0; next LINE };

# WRONG _ need to check that ! is first character in line
         if($include_status) { $line =~ s/\!// } # Strip first ! only
         if(not $exclude_status) {
            print MODIFIED_FILE "$line";
         }

         if($line =~ m/$END_EXCLUDE_TAG/) { $exclude_status = 0};
      } 
      close(MODIFIED_FILE);
      if($nodryrun) {rename($directory_name.$file_name."-stripped",$directory_name.$file_name)};

   }

   #==================================================================#
   # Preprocess every file for #define PROJECT tags                   #
   #==================================================================#
   @local_filenames = &get_filenames_in_directory($directory_name);
   if($verbose) {print "REMAINING: ".join("\t",@local_filenames)."\n"}; 

   FILE_PREPROCESS: foreach $file_name (@local_filenames) { 
      if($verbose) {print "... Testing file $file_name \n"};
      #
      # Skip the special or exclude files (already GPL processed?)
      #
      if("$exclude_files.$manual_preprocess_files.$OBJECTS_FILE" =~ m/$file_name/) {
            &io("\n... Skipping preprocess of $file_name"); next FILE_PREPROCESS;
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
      if($nodryrun) {rename($directory_name.$file_name."-stripped",$directory_name.$file_name)};
   }
} # directory loop
#
# Clean up
#
&io("\n\n ============= REPORT =================\n");
if($number_selected_projects){ 
   &io("yambo GPL +  $selected_projects_string source created successfully.\n");
}
else { &io("yambo GPL only created successfully.\n") }
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
&io(" ---> Run batchfiles for deleting svn entries: svndelete.batch and delete.batch \n");
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
  chomp(@gpl_only_source_files);
  return @gpl_only_source_files;
}
#---------------------------------------------------------------------#
# This will get all the filenames in the directory 
# excluding subdirectories (e.g. .svn, .git),
# and binary files (in case some are left over)
#---------------------------------------------------------------------#
sub get_filenames_in_directory{
  my $directory_name = shift(@_);
  my @RealNames = ();
  opendir(DIR, $directory_name) or die "Cannot opendir : $!";
  my @Names = readdir(DIR);
  foreach $name (@Names) {
#    if ($name =~ /^\.\.?$|$SVN_DIR/){ next; }
#    if ($name =~ /^\.\.?$|$GIT_DIR/){ next; }
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
sub ior{
    my($io_string) = shift(@_);
    printf "%10s","$color_start{red} $io_string $color_end{red}";
    print LOG $io_string;
}
sub iog{
    my($io_string) = shift(@_);
    printf "%10s","$color_start{green} $io_string $color_end{green}";
    print LOG $io_string;
}
sub ioc{
    my($io_string) = shift(@_);
    printf "%-60s",$io_string;
    print LOG $io_string;
}
#---------------------------------------------------------------------#
sub io2{
    my($io_string) = shift(@_);
    my($result) = shift(@_);
#   printf "%-60s%-10s\n" substr($io_string,0,60),"$color_start{red} $result $color_end{red}";
#   printf "%60s %10s\n",substr($io_string,0,60),"$color_start{red} $result $color_end{red}";
    printf "%60s %10s\n",$io_string,"$color_start{red} $result $color_end{red}";
    print LOG $io_string;
}
#---------------------------------------------------------------------#
# Checks the text passed via the list @_ for the presence
# of #define PROJECT statements.
# If no #defines are present at all, or there is no match with
# a user-defined project, then returns $match_project = 0.
#
# Output: $match_project
#         0 if no #defines are present at all
#         0 if no #define PROJECT are present at all
#        -n if matches to n selected project
#        +n if matches to n projects
#
#---------------------------------------------------------------------#
sub check_for_project_preprocess_flags{
    my @source_contents = @_;
    my $match_project = 0;
    my @match = grep(/\sdefined|ifdef|\!defined/,@source_contents);
    if($#match+1 == 0) { return $match_project };  # No #define present at all

    foreach my $selected_project (@selected_projects_list){   
       $match_project -= grep(/$selected_project/,@match);  
    }
    if($match_project lt 0) { return $match_project };  # Matches to any selected project

    foreach my $project (@available_projects) {
       $match_project += grep(/$project/,@match);  # only #define DOUBLE, etc are presesnt
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
#   &io("\n... Preprocessing file $file_name: ");
    &ioc("\n... Preprocessing file $file_name: ");
    
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
             # If the line ALSO matches any selected project, leave it alone!
             #
             foreach my $selected_project (@selected_projects_list) {
                if($myline =~ m/$selected_project/ ) { last DEFPROJ };
             }

             $inclusive_loop = 1;
             $report_file_change = 1;
             next LINE;
          }
          #
          # Turn on the exclusive stripping
          # this probably needs to be skipped for a selected project match
          #
          if($myline =~ m/#(\w| )*!defined|# *ifndef/ and $myline =~ m/$project/ ) { 
             #
             # If the line ALSO has a core project, leave it alone!
             #
             foreach $core_project (@core_projects) {
                if($myline =~ m/$core_project/ ) { last DEFPROJ };
             }
             #
             # If the line ALSO matches any selected project, leave it alone!
             #
             foreach my $selected_project (@selected_projects_list) {
                if($myline =~ m/$selected_project/ ) { last DEFPROJ };
             }
             $exclusive_loop = 1; # 
             $report_file_change = 1;
             next LINE;
          }
       }

       if($myline =~ m/#(\w| )*defined|# *ifdef/ ) { $preprocess_loop += 1 }
       if($inclusive_loop and $project_else == 0) { next };

       push(@preprocessed_lines,$myline);

    } # end loop LINE
#   if($report_file_change) {&io(" $color_start{red} CHANGED  $color_end{red} ")};
    if($report_file_change) {&ior("CHANGED")};
    if(not $report_file_change) {&iog("unchanged")};
#   if($report_file_change) {print "... Preprocessed file $file_name \n"}
    return @preprocessed_lines;
}

sub usage {

 print <<EndOfUsage

   Syntax: yamboo.pl [OPTIONS]

           [OPTIONS] are:
                   -v                 Verbose output
                   -d                 Perform dry run
                   -p [PROJECTS]      List of projects to include in source

            [PROJECTS] has form: "GPL" or
                                 "PROJECT1 PROJECT2 .."
EndOfUsage
  ;
}

