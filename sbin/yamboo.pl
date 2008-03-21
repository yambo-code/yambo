#!/usr/bin/perl -w
#
# yamboo.pl
# Create source code tree for a particular PROJECT
# Usage:
# perl mergexml.pl -p="PROJECT"
# C. Hogan, 2008
# Version 1.5
#
#
# TODO
# Tidy up source!
# Check for existence of project in list
#

use Getopt::Long;
use File::Find;

&GetOptions("help"           => \$help,
            "p=s"          => \$project_list);

#
# Define special flags and stuff
#
@special_files = ('X_os.F');
@exclude_files = ('yamboo.pl');
@core_projects = ('_yambo','_ypp','_p2y','_a2y','_f2y','_e2y');
@user_projects = ('_RAS','_REELS','_BIGSYS','_ELPH','_SC','_BS_CPL','_SPP_ELPH','_SPP_RAS');
#
# Exclude #defines like _SC_KERNEL by adding a space or newline character
#
@tmp1 = map { $_." " } @user_projects;
@tmp2 = map { $_."\n" } @user_projects;
@available_projects = (@tmp1,@tmp2);
#
# User interface
#
if($help ){ print "Syntax: yamboo.pl -p=\"PROJECT\" \n        where PROJECT is $available_projects"."\n" and exit};
if($project_list){ 
   print "Processing yambo for $project_list ...\n\n";
   print "Sorry - not scripted yet. Run without PROJECT for GPL script.\n";
   exit; 
}
else{              
   print "Processing yambo for GPL ...\n\n";
}
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
# Open the log file for writing
#
open(LOG, ">yamboo.log") or die "Error opening log\n";
#
# Get list of all directories, recursively
#
find sub {
   return unless -d $_ ;
   $directory_name = $File::Find::name; 
   if ($directory_name =~ m/$SVN_DIR/){ return }  # Strip .svn dirs
#  foreach $project_directory (@project_directories) {
#    if($directory_name =~ m/$project_directory/) { return }
#  }
   push (@directories_list, $directory_name."/");
}, ".";
#
# Process each directory: main loop.
#
#push(@temp_list,"src/zzzzz/");
#foreach $directory_name (@temp_list) {

foreach $directory_name (@directories_list) {
   print "Processing directory $directory_name \n";
   print LOG "Processing directory $directory_name \n";

   @local_filenames = &get_filenames_in_directory($directory_name);

   print LOG "... Files: ".join("\t",@local_filenames)."\n"; 

   #
   # GPL_OBJECT present: clean directory of not listed files
   #
   if (grep(/$GPL_OBJECTS_FILE/, @local_filenames) ){
      print "... Found $GPL_OBJECTS_FILE file:\n";

      open(GPL_OBJ_FILENAME, $directory_name.$GPL_OBJECTS_FILE) 
         or die "Error opening $directory_name.$GPL_OBJECTS_FILE\n";
      @gpl_objects_contents = <GPL_OBJ_FILENAME>;
      close(GPL_OBJ_FILENAME);
      #
      # Get the GPL filename list
      #
      @gpl_only_source_files = &get_source_from_object_list(@gpl_objects_contents);
      #
      # If no objects listed, flag the directory for deletion
      #
      if($#gpl_only_source_files+1 == 0) {
         push(@directories_for_deletion,$directory_name);
         print "... Directory marked for deletion\n"; 
         next;
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
            print "... Deleting non-GPL file: $file_name\n";
            unlink ($directory_name.$file_name); 
#           push (@delete_list, $directory_name.$file_name);
#           unlink (@delete_list); 
         }
      }
      #
      # Move the .objects_gpl to .objects
      #
      rename($directory_name.$GPL_OBJECTS_FILE,$directory_name.$OBJECTS_FILE);
   }

   @local_filenames = &get_filenames_in_directory($directory_name);
   print LOG "REMAINING: ".join("\t",@local_filenames)."\n"; 
#  print "REMAINING: ".join("\t",@local_filenames)."\n"; 

   #
   # Process individual files for GPL tags
   #
FILE_GPL: foreach $file_name (@local_filenames) { 

      foreach $exclude_file (@exclude_files) {
         if($file_name =~ m/$exclude_file/) {
            print "... Skipping $file_name\n";
            next FILE_GPL;
         }
      }
#     print "... Processing individual file $file_name \n";
      #
      # Test if file is a _gpl file
      #
      if($file_name =~ m/$GPL_FILE_SUFFIX/) {
         print "... Reverting GPL only $file_name file\n";
         $new_file_name = $file_name;
         $new_file_name =~ s/$GPL_FILE_SUFFIX//;
         rename($directory_name.$file_name,$directory_name.$new_file_name);
         next;
      }
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
      print "... Processing GPL tagged file $file_name\n";
      #
      # Strip non GPL lines
      #
      open(MODIFIED_FILE, ">", $directory_name.$file_name."-stripped") 
         or die "Error opening $directory_name.$file_name -stripped\n";
      $exclude_status = 0;
      $include_status = 0;
      while($line = shift(@source_contents)) {
         #
         # Test to see if we are inside a GPL loop
         #
         if($line =~ m/$START_EXCLUDE_TAG/) { $exclude_status = 1};
         if($line =~ m/$START_INCLUDE_TAG/) { $include_status = 1; next };
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

   #
   # Process individual files for PROJECT tags
   #
   @local_filenames = &get_filenames_in_directory($directory_name);
#  print "REMAINING: ".join("\t",@local_filenames)."\n"; 

   FILE_PREPROCESS: foreach $file_name (@local_filenames) { 
#     print "... Testing file $file_name \n";
      #
      # Skip the special files (already GPL processed)
      #
      foreach $special_file (@special_files) {
         if($file_name =~ m/$special_file/) {
            print "... Skipping $file_name\n";
            next FILE_PREPROCESS;
         }
      }
      foreach $exclude_file (@exclude_files) {
         if($file_name =~ m/$exclude_file/) {
            print "... Skipping $file_name\n";
            next FILE_PREPROCESS;
         }
      }
      #
      # Read contents of source file
      #
      open(SOURCE_FILE, $directory_name.$file_name) 
         or die "Error opening $directory_name.$file_name\n";
      @source_contents = <SOURCE_FILE>;
      close(SOURCE_FILE);
      #
      # Skip if no preprocessing needed
      #
      @match = grep(/\sdefined|ifdef/,@source_contents);
      if($#match+1 == 0) { next FILE_PREPROCESS };  # No #define present
      $match_project = 0;
      foreach $project (@available_projects) {
         $match_project += grep(/$project/,@match);
      }
      if($match_project == 0) { next FILE_PREPROCESS }; # No #define PROJECT present

      open(MODIFIED_FILE, ">", $directory_name.$file_name."-stripped") 
         or die "Error opening $directory_name.$file_name -stripped\n";
      #
      # Strip projects
      #
      $inclusive_loop = 0;
      $exclusive_loop = 0; 
      $report_file_change = 0;
      $preprocess_loop = 0;
      $project_else = 0;
LINE: while($line = shift(@source_contents)) {
#        print "XXX $preprocess_loop $inclusive_loop $exclusive_loop $line";
         #
         # Test to see if we are inside a PROJECT loop
         #
         if($exclusive_loop and ($preprocess_loop == 0) and $line =~ m/# *endif/ ) { $exclusive_loop = 0; next };
         if($inclusive_loop and ($preprocess_loop == 0) and $line =~ m/# *endif/ ) { $inclusive_loop = 0; $project_else = 0; next };
         if($preprocess_loop gt 0 and $line =~ m/# *endif/ ) { $preprocess_loop += -1 };
         if($inclusive_loop and $line =~ m/# *else/ ) { $project_else = 1; next };
            #
            # Turn on the inclusive stripping
            #
            DEFPROJ: foreach $project (@available_projects) {
               # #(\w| )* means # followed by zero or more (*) characters (\w) or spaces
#              print "$project ";

               if($line =~ m/#(\w| )*defined|# *ifdef/ and $line =~ m/$project/ ) { 
                  #
                  # If the line ALSO has a core project, leave it alone!
                  #
                  foreach $core_project (@core_projects) {
                     if($line =~ m/$core_project/ ) { last DEFPROJ };
                  }
                  $inclusive_loop = 1;
                  $report_file_change = 1;
#                 print "START  PROJECT $project\n";
                  next LINE;
#                 last DEFPROJ;
               }

            #
            # Turn on the exclusive stripping
            #
#           foreach $project (@available_projects) {
               if($line =~ m/#(\w| )*!defined|# *ifndef/ and $line =~ m/$project/ ) { 
                  $exclusive_loop = 1; # 
                  $report_file_change = 1;
                  next LINE;
               }
#           }
            }

            if($line =~ m/#(\w| )*defined|# *ifdef/ ) { $preprocess_loop += 1 }

            if($inclusive_loop and $project_else == 0) { next };
            print MODIFIED_FILE "$line";
      }
      if($report_file_change) {print "... Preprocessing file $file_name \n"}
      close(MODIFIED_FILE);
      rename($directory_name.$file_name."-stripped",$directory_name.$file_name);
   }
      #

} # directory loop
print "\n yambo GPL created successfully.\n";
#
# Delete project only directories (check if SVN still works)
#
print "\n Remaining user tasks:\n";
foreach $directory (@directories_for_deletion) {
   print " ---> Delete project directory $directory \n";
## rmdir($project_directory);
}
foreach $special_file (@special_files) {
   print " ---> Manually preprocess file $special_file \n";
}
exit;

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

##########################################################
sub get_source_from_object_list{
  my @file_contents = @_;
  my(@line_elements,@objects,$element);
# print @file_contents;
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


##########################################################
#
# This will get all the filenames in the directory 
# excluding subdirectories (e.g. .svn),
# and binary files (in case some are left over)
#
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

