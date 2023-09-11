#!/usr/bin/perl

use strict;
use FindBin;
use lib "$FindBin::Bin/";
# Read the configuration script
BEGIN { require "$FindBin::Script.config"; };
use lib "$::cilhome/lib"; # The libraries are in the lib directory

use Cilly;
my $stub = CilCompiler->new(@ARGV);
$stub->setVersion();
$stub->doit();


package CilCompiler;
use strict;
BEGIN {
    @CilCompiler::ISA = qw(Cilly);
    # default to using the native compiler
    $CilCompiler::otter_native = "$FindBin::Bin/$::native";
    $CilCompiler::otter_byte = "$FindBin::Bin/$::byte";
    $CilCompiler::do_default = "$::do_default";

    # use the the native version of otter unless asked otherwise
    $CilCompiler::otter = $CilCompiler::otter_native;

    if (not $::compile_after_merge) {
        $ENV{CILLY_DONT_COMPILE_AFTER_MERGE} = "";
    }

    # always print a backtrace upon abort
    $ENV{"OCAMLRUNPARAM"} = "b" . $ENV{"OCAMLRUNPARAM"};
}

sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    if($arg eq '--bytecode') {
        $CilCompiler::otter = $CilCompiler::otter_byte;
        return 1;
    }
    if($arg =~ /^--do/) {
        $self->{DOCOMMAND} = $arg;
    }
    if($arg =~ /^--merge$/) {
        $self->{DOMERGE} = 1;
    }
    return $self->SUPER::collectOneArgument($arg, $pargs);
}

sub usage {
    print "Usage: $0 [options] [gcc_or_mscl arguments]\n";
}

sub helpMessage {
    my($self) = @_;
    $self->SUPER::helpMessage();
    print <<EOF;

The following are the arguments of the Cilly process. For arguments with
parameters, put '=' between the arguments and the parameter, e.g.,
'--option=param'.

EOF
   my @cmd = ($CilCompiler::otter, '-help');
   $self->runShell(@cmd); 
}

sub CillyCommand {
    my ($self, $ppsrc, $dest) = @_;

    my $aftercil;
    my @cmd = ($CilCompiler::otter);

    if((not defined $self->{DOCOMMAND}) && (not defined $self->{DOMERGE})) {
        @cmd = ($CilCompiler::otter, $CilCompiler::do_default);
    }
    if(defined $self->{CILLY_OUT}) {
        $aftercil = new OutputFile($dest, $self->{CILLY_OUT});
        return ($aftercil, @cmd);
    }
    $aftercil = $self->cilOutputFile($dest, 'cil.c');
    return ($aftercil, @cmd, '--out', $aftercil);
}

sub MergeCommand {
    my ($self, $ppsrc, $dir, $base) = @_;

    return ('', $CilCompiler::otter);
}


1;
