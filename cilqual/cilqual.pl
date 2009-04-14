#!/usr/bin/perl

use strict;
use FindBin;
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
    $CilCompiler::cil_native = "$FindBin::RealBin/$::native";
    $CilCompiler::cil_byte = "$FindBin::RealBin/$::byte";
    $CilCompiler::do_default = "$::do_default";

    # use the most recent version of cil, or whichever is available
    $CilCompiler::mtime_native = int((stat($CilCompiler::cil_native))[9]);
    $CilCompiler::mtime_byte = int((stat($CilCompiler::cil_byte))[9]);
    if($CilCompiler::mtime_native < $CilCompiler::mtime_byte) {
        $CilCompiler::cil_native = $CilCompiler::cil_byte;
    }

    if (not $::compile_after_merge) {
        $ENV{CILLY_DONT_COMPILE_AFTER_MERGE} = "";
    }
}

sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    if($arg eq '--bytecode') {
        $CilCompiler::cil_native = $CilCompiler::cil_byte;
        $ENV{"OCAMLRUNPARAM"} = "b" . $ENV{"OCAMLRUNPARAM"};
        return 1;
    }
    if($arg =~ /^--do/) {
        $self->{DOCOMMAND} = $arg;
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
   my @cmd = ($CilCompiler::cil_native, '-help');
   $self->runShell(@cmd); 
}

sub CillyCommand {
    my ($self, $ppsrc, $dest) = @_;

    my $aftercil;
    my @cmd = ($CilCompiler::cil_native);

    if(not defined $self->{DOCOMMAND}) {
        @cmd = ($CilCompiler::cil_native, $CilCompiler::do_default);
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

    return ('', $CilCompiler::cil_native);
}


1;
