#!/usr/bin/perl

use strict;
use Data::Dumper;
use FindBin;
use lib "$FindBin::RealBin";
# Read the configuration script
use CilQualConfig;
use lib "$::cilhome/lib"; # The libraries are in the lib directory

use Cilly;

$::default_is_merge = 0;
my $stub = CilCompiler->new(@ARGV);

$stub->setVersion ();

# print Dumper($stub);
$stub->doit();


# Define here your favorite compiler by overriding Merger methods
package CilCompiler;
use File::Basename;
use strict;
BEGIN {
    @CilCompiler::ISA = qw(Cilly);
    $CilCompiler::compiler = "$FindBin::RealBin/runcilqual.native";
    $CilCompiler::byte = "$FindBin::RealBin/runcilqual.d.byte";

    # Use the most recent version of cilly
    $CilCompiler::mtime_native = int((stat($CilCompiler::native))[9]);
    $CilCompiler::mtime_byte = int((stat($CilCompiler::byte))[9]);
    if($CilCompiler::mtime_native < $CilCompiler::mtime_byte) {
        $CilCompiler::compiler = $CilCompiler::byte;
    }

    $ENV{CILLY_DONT_COMPILE_AFTER_MERGE} = "";
}

# We need to customize the collection of arguments
sub collectOneArgument {
    my($self, $arg, $pargs) = @_;
    if($arg eq '--bytecode') {
        $CilCompiler::compiler = $CilCompiler::byte;
        $ENV{"OCAMLRUNPARAM"} = "b" . $ENV{"OCAMLRUNPARAM"};
        return 1;
    }
    if($arg =~ /^--do/) {
        $self->{DOCOMMAND} = $arg;
    }
    # See if the super class understands this
    return $self->SUPER::collectOneArgument($arg, $pargs);
}

sub usage {
    print "Usage: $0 [options] [gcc_or_mscl arguments]\n";
}

sub helpMessage {
    my($self) = @_;
    # Print first the original
    $self->SUPER::helpMessage();
    print <<EOF;

  All other arguments starting with -- are passed to the Cilly process.

The following are the arguments of the Cilly process
EOF
   my @cmd = ($CilCompiler::compiler, '-help');
   $self->runShell(@cmd); 
}


sub CillyCommand {
    my ($self, $ppsrc, $dest) = @_;

    my $docommand;
    my $aftercil;
    my @cmd = ($CilCompiler::compiler);

    if(not defined $self->{DOCOMMAND}) {
        @cmd = ($CilCompiler::compiler, '--docilqual');
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

    return ('', $CilCompiler::compiler);
}


1;
