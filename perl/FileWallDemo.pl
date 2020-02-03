#!/usr/bin/perl
#
#	FileWallDemo.pl
#
#	FileWall access Demo application 
#

use strict;
use warnings;

use Getopt::Long 'HelpMessage';;
use Data::Dumper;

# To use module, located in the lib subdirectory. Can be omitted after module 
# is installed into Perl's library directory 
use File::Basename;

use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0) . '/perl/lib';

# First, include fileWall support modile
use FileWall;


my $verbose;

# define API key. Here demo API is set which is used in this
# application by default. You can override it in the command line
my $api = "you api key";

# Text file, placed in the current directory. Override it in the
# command line. Full or relative path can be used
#my $filename = "FileWallTestTextFile.txt";

# destination directory or output file. Can be empty.
my $outfile;

 GetOptions ("src=s" => \ my $filename,
            "api=s"   => \$api,
            "dst=s"    => \$outfile,
             "verbose"  => \$verbose
             );

if (!$filename) {
    printHelp();
    exit(1);
}

if ($verbose) {
    print "$0 :\n";
    print "API key          :   $api\n";
    print "source file      :   $filename\n";
    if ($outfile && length($outfile) > 0) {
        print "destination      :   $outfile\n";
    }
    print "\n * * * FileWall module taken from: * * *\n";
    print($INC{"FileWall.pm"}, "\n");
    print "\n\n";
}

my $fileWall = FileWall::new(api_key => $api) || die "Cannot create FileWall instance";

# Below - example, how to get particular field from the object' instance
#print $fileWall->{api_key}."\n";

# Use the line below to get full list of object properties
# print Dumper($fileWall);

 print "Initial status - ".$fileWall->{status}."\n" if $verbose;

my $autorized = $fileWall->authorize();
if ($autorized) {
    print "Authorised Status - ".$fileWall->{status}."\n" if $verbose;
    # upload file
    my $uploaded = $fileWall->upload($filename);
    if ($uploaded) {
        print "file $filename succesfully uploaded\n" if $verbose;
        my $downloadLink = $fileWall->poll();
        if ($fileWall->{hasError}) {
            &printError();
            die "Cannot get download link";
        } else {
            if ($verbose) {
                print "File processed on FileWall server\n";
                print "Download link -> $downloadLink \n";
                print "Source file name -- ".$fileWall->{sourceFileName}."\n";
                print "Output file name -- ".$fileWall->{outputFileName}."\n";
                print "Output file type -- ".$fileWall->{outputType}."\n";
            }
            # Now download processed data and store it into new file
           # When method is called without arguments, current/working directory and default
           # filename is used
            my ($downloadFileName, $fileData) = $fileWall->download($outfile);
            if ($downloadFileName) {
                my $saved = &saveFile($downloadFileName, $fileData);
                if ($saved) {
                    print "Downloaded file saved to $saved\n" if $verbose;
                } else {
                    print "Cannot write  file  $saved\n" if $verbose;
                }
            } else {
                &printError();
            }
        }
    } else {
        print "Cannot upload $filename\n" if $verbose;
        &printError();
        die "Error during upload $filename";
    }
} else {
    print "Authorisation failed!\n" if $verbose;
    &printError();
    die "Cannot autorize in Filewall service";
}

sub printError
{
    print "Error code - ".$fileWall->{has_error}."\n";
    print "Error message - ".$fileWall->{error_string}."\n";
}

sub saveFile
{
    my ($pathname, $fileData) = @_;
    my $dirname = dirname $pathname;
    while (-f $pathname) {
        print "File exists - $pathname\n" if $verbose;
        # output file does.exist. Trying to generate new name
        # To preserve OS' ability work with files by extensions, we  should add
        # index "inside" file name
        my $basename = basename $pathname;
        my @parts = split /\./, $basename;
        my $newIndex = 1;
        my $l = scalar(@parts);
        if ( $l == 1) {
            $basename = $basename.'.1';
        } elsif ($l == 2) {
            my ($value) = $parts[1] =~ /^(\d+)$/;
            if (! $value) {
                $value = 1;
            } else {
                $value++;
            }
            $basename = $parts[0].".".$value.".".$parts[1];
        } else {
            # need to check alement "near the end"
            my $index = $parts[$l - 2];
            # if this element is a number, this is out index. Increment it
            if (my ($value) = $index =~ /^(\d+)$/) {
                $newIndex = $value + 1;
            }
            $parts[$l - 2] = $newIndex;
            $basename = join('.',@parts);
        }
        $pathname = $dirname."/".$basename;
    }
        print "Output file  - $pathname\n" if $verbose;
    open my $fh, '>:raw', $pathname;
    if ($fh) {
        print $fh $fileData;
        close $fh;
        return $pathname;
    }
    return '';
}


sub printHelp {
    print "Usage :\n\n";
    print "$0 --src <source_file> [--api <API_KEY>] [ --dst <destination>] [--verbose]\n\n";
    print "  - source_file     -   text file to be processed (required)\n";
    print "  - API_KEY         -   API key for FileWall service (demo account used by default)\n";
    print "  - destination     -   Destination filename or directory\n";
    print "  - verbose         -   verbose mode on\n\n";
    exit(1);
}

=head1 NAME

FileWallDemo.pl - converts text file to PDF with FileWall service

=head1 SYNOPSIS



--src               path to source file to be converted (required)
--api              API key  (demo api is used bydefault)
--dst              Destination filename (or directory), can be ommited
--verbose,-v    Verbose mode - logs all steps to STDOUT
--help,-h       Print this help

=head1 VERSION

1.0

=cut

