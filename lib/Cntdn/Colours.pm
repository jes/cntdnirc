package Cntdn::Colours;

use strict;
use warnings;

our @ISA = qw(Exporter);
our @EXPORT = qw(COLOUR RESET BOLD);

# https://github.com/myano/jenni/wiki/IRC-String-Formatting
sub COLOUR {
    my ($fg, $bg) = @_;

    my %code = (
        white => '00',
        black => '01',
        blue => '02',
        green => '03',
        red => '04',
        brown => '05',
        purple => '06',
        orange => '07',
        yellow => '08',
        lightgreen => '09',
        teal => '10',
        cyan => '11',
        royalblue => '12',
        pink => '13',
        grey => '14',
        silver => '15',
    );

    return "\x03$code{$fg},$code{$bg}";
}

sub RESET {
    return "\x0F";
}

sub BOLD {
    return "\x02";
}

sub ITALIC {
    return "\x1D";
}

sub UNDERLINE {
    return "\x1F";
}

sub INVERSE {
    return "\x16";
}

1;
