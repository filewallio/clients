package FileWall;

use strict;
use warnings;

use HTTP::Request ();
use Data::Dumper;
use LWP;
use File::Basename;
use JSON::XS;

    BEGIN {
        use Exporter ();
        use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
        $VERSION     = v1.0;
        @ISA         = qw(Exporter);
        #Give a hoot don't pollute, do not export more than needed by default
        @EXPORT      = qw();
        @EXPORT_OK   = qw();
        %EXPORT_TAGS = ();
        $ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;
    }


    my $api;        # JSON API regerence
    my $agent;      # LWP based agent
    # Authorisation related data
    my $uid;        # identifier received for given API Key

    my $selfLink;       # link to processing node
    my $uploadLink;     # link to upload source file
    my $downloadLink;   #link to download procesed file

    sub new {
        my ($class, $api_key, %parameters) = @_;
        return undef unless $api_key;

            my %ua_opts = %parameters;

            my $self = bless ({
                api_key         => $api_key,
                has_error       => 0,
                error_string    => '',
                status          => 'Not connected',
                debug           => $ua_opts{debug},
                outputFileName  => '',
                sourceFileName  => '',
                outputType      => '',
            }, ref ($class) || 'FileWall');
            $agent = LWP::UserAgent->new();
 #           $api = JSON::new();
        return $self;
     }


    sub authorize {
        my $self = shift;
        my $url = 'https://filewall.io/api/authorize';
        my $header = ['APIKEY' => $self->{api_key}];
         my $r = HTTP::Request->new('GET', $url, $header, '');
        my $res = $agent->request($r);
        my $rc = $res->code;
         if ($res->is_success) {
            if ($rc != 202 ) {
                $self->{has_error} = $rc;
                $self->{error_string} = $res->message;
                return 0;
            } else {
                my $retJSON = $res->content;
                my %retValue = %{decode_json($retJSON)};
                $self->{status} = $retValue{status};
                $uid = $retValue{uid};
                my %links = %{$retValue{"links"}};
                $selfLink = $links{self};
                $uploadLink = $links{upload};
           }
           return 1;
        } else {
           $self->{has_error} =  $rc;
           $self->{error_string} = $res->message;
           return 0;
        }
    }

    sub upload {
        my ($self,$filename) = @_;
        if (-e $filename) {
            my $header = ['filename' => $filename, 'Content-Type' => 'application/octet-stream'];
            open FILE, $filename;
            my $content = <FILE>;
            close FILE;
            my $r = HTTP::Request->new('POST',$uploadLink, $header, $content);
            my $res = $agent->request($r);
            my $rc = $res->code;
            if ($rc == 202 ) {
                return 1;
            } else {
                my %content = $res->content;
                my $msg = $content{"error"};
                $self->{has_error} = $rc;
                $self->{error_string} = ($msg ? $msg : $res->message);
                return 0;
            }
        } else {
            $self->{has_error} = 10001;
            $self->{error_string} = "$filename doesn't exist";
            return 0;
        }
    }

    sub poll {
        my ($self) = @_;
        my $header = ['APIKEY' => $self->{api_key}];
        my $r = HTTP::Request->new('GET', $selfLink, $header, '');
        $self->{has_error} = 0;
        $self->{error_string} = '';
        $self->{outputFileName} = '';
        my $attemptCount = 0;
        while ($attemptCount < 100) {
            my $res = $agent->request($r);
            my $rc = $res->code;
            if ($rc == 200) {
               my $retJSON = $res->content;
               my %retValue = %{decode_json($retJSON)};
               my $status = $retValue{status};
               if ($status eq "waiting" || $status eq "processing") {
                   # task is in the queue or processing, we need to wait more
                   $attemptCount++;
                   sleep(3);
               } elsif ($status eq "archived") {
                   $self->{has_error} = 10002;
                   $self->{error_string} = "processing finished successfullly, converted file is NO LONGER available for download";
                  return "";
               } elsif ($status eq "finished") {
                   # processing is finished, check for download link
                   my %links = %{$retValue{"links"}};
                   $downloadLink = $links{"download"};
                   if ($downloadLink && $downloadLink ne "") {
                       $self->{has_error} = 0;
                       $self->{outputFileName} = $retValue{"name"};
                       $self->{sourceFileName} = $retValue{"source_name"};
                       $self->{outputType} = $retValue{"type"};
                      return $downloadLink;
                    } else {
                       $self->{has_error} = 10003;
                       $self->{error_string} = "No download link";
                       return '';
                    }
                } else {
                   # non processed status
                   $self->{has_error} = 10004;
                   $self->{error_string} = "Poll error - ".$status;
                   return '';
                 }
            } else {
                my %content = $res->content;
                my $msg = $content{error};
                $self->{has_error} = $rc;
                $self->{error_string} = ($msg ? $msg :$res->message);
                return '';
            }
        }
        $self->{has_error} = 10005;
        $self->{error_string} = "Poll error - timeout error";
        return '';
   }

    sub download {
        my ($self, $pathname) = @_;
        $self->{has_error} = 0;
        $self->{error_string} = '';
        my $header = [];
        # First, we should prepare downloading directory and check
        # if it is available for writing
        if ($pathname) {
             # it can be full pathname to file
             my($filename, $path, $suffix) = fileparse($pathname);
             unless(-e $path) {
                 $self->{has_error} = 10010;
                 $self->{error_string} = "Cannot create output directory $path";
                 return ();
             }
            # finally append server provide filename if user provided
            # directory name only
            if (!$filename) {
                $pathname = $path."/".$self->{outputFileName};
            }
            if (not -w $path) {
                $self->{has_error} = 10011;
                $self->{error_string} = "File $pathname is not available for writing";
                return ();
            }
       }

        my $r = HTTP::Request->new('GET', $downloadLink, $header, '');
        my $res = $agent->request($r);
        my $code = $res->code;
        if ($code == 200) {
                # extract binary data from response
                my $fileData = $res->content;
                if (!$pathname || length($pathname) == 0) {
                    # default case use current (default/working) directory
                    # and filename supplied by server
                    $pathname = $self->{outputFileName};
                 }
                return ($pathname, $fileData);
        } else {
            my %content = $res->content;
            my $msg = $content{error};
            $self->{has_error} = $res->code;
            $self->{error_string} = ($msg ? $msg : $res->message);
            return ();
        }
    }


1; # OK to loader


