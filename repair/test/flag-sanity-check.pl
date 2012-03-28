#!/usr/bin/perl

#Author: Neal Holtschulte
#Contact: neal.holts@gmail.com
#Purpose: We suspected that the --help flag for genprog was not printing out all the same flags as the program was using so this script was created to test the behavior.

if ($#ARGV != 2 ) {
	print "3 arguments are required. The name of file 1, from which to get flags, the name of file 2, which will be checked to make sure it contains the flags found in file 1, and a boolean (zero or one) which is true if lines in the first text file can fail to match the flag regular expression.\n";
	exit(1);
}

################### CONSTANTS ##################################
my $fileOne=$ARGV[0];
my $fileTwo=$ARGV[1];

my $allowMatchFail=$ARGV[2];

#Column headers for the output
my @col_headers = ('Day','Date','Duration','Text');

my $reg_exps = '^--(.*?)\s';

sub findString{
	($str, @in) = @_;
	foreach my $line (@in){
		$temp = index($line, $str);
	        if($temp != -1){ #if found...
			return 1;
		}
	}
	return 0;
}

################# PROCESSING ###################################
# http://www.perlfect.com/articles/perlfile.shtml
open FILE, "<$fileOne" or die $!;		# Open for reading
my @lines1 = <FILE>;			# Store the lines in an array
close(FILE);				# Close the file

open FILE, "<$fileTwo" or die $!;	# Open for reading
my @lines2 = <FILE>;			# Store the lines in an array
close(FILE);				# Close the file

#iterate through the lines of the infile
foreach my $line (@lines1){

        #match to the current reg exp
        if($line =~ /$reg_exps/){
		#print "$1\n";
		$temp = findString($1, @lines2);
		if(!$temp){
		    print "'$1' not found in $fileTwo\n";
			exit(1);
		}
        #otherwise there may be an error.
        }elsif(!$allowMatchFail){
		print "'$line'\nDid not match the regular expression\n";
		exit(1);
	}
}

exit(0);
