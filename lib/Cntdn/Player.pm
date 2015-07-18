package Cntdn::Player;

use strict;
use warnings;

# nick
# letters_length
# letters_word
# numbers_sum
# numbers_expr
# score

sub new {
    my ($pkg, %opts) = @_;

    my $self = bless \%opts, $pkg;

    return $self;
}

1;
