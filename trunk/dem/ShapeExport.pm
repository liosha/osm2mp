package ShapeExport;

use strict;
use Carp;

use CAM::DBF;

my $INF = 10e20;

my %shapetype = (
    NULL        => 0,
    POINT       => 1,
    POLYLINE    => 3,
    POLYGON     => 5,
);


sub new {
    my $class = shift;
    my $self  = {};

    $self->{NAME} = shift;
    $self->{SHPSIZE} = 50;
    $self->{SHXSIZE} = 50;
    $self->{RCOUNT} = 0;

    $self->{XMIN} = $INF;
    $self->{XMAX} = -$INF;
    $self->{YMIN} = $INF;
    $self->{YMAX} = -$INF;
    
    $self->{TYPE} = $shapetype{uc shift};
    croak 'Shapefile type not specified'        if !$self->{TYPE};

    open (my $shp, '>', $self->{NAME}.'.shp');
    croak "Can't create .shp"   if !$shp;
    binmode $shp;
    print $shp pack("Nx[20]NLLddddx[32]",( 9994,$self->{SHPSIZE},1000,$self->{TYPE},$self->{XMIN},$self->{YMIN},$self->{XMAX},$self->{YMAX} ));
    $self->{SHP} = $shp;

    open (my $shx, '>', $self->{NAME}.'.shx');
    croak "Can't create .shx"   if !$shx;
    binmode $shx;
    print $shx pack("Nx[20]NLLddddx[32]",( 9994,$self->{SHXSIZE},1000,$self->{TYPE},$self->{XMIN},$self->{YMIN},$self->{XMAX},$self->{YMAX} ));
    $self->{SHX} = $shx;

    my $dbf = CAM::DBF->create($self->{NAME}.'.dbf', @_);
#    $dbf->writeHeader();
    $self->{DBF} = $dbf;


    bless ($self, $class);
    return $self;
}





sub write {
    my $self = shift;

    my ($xmin,$ymin,$xmax,$ymax) = ($INF,$INF,-$INF,-$INF);

    my $data = shift;
    my $rdata = q{};

    ##  null
    if ( $self->{TYPE} == $shapetype{NULL} ) {
        $rdata = pack( 'L', $self->{TYPE} );
    }

    ##  point
    if ( $self->{TYPE} == $shapetype{POINT} ) {
        $rdata = pack( 'Ldd', $self->{TYPE}, @$data );
    }

    ##  polyline or polygon
    if ( $self->{TYPE} ~~ [ @shapetype{'POLYLINE','POLYGON'} ] ) {
        my $rpart = q{};
        my $rpoint = q{};
        my $ipoint = 0;
        for my $line (@{$data}) {
            $rpart .= pack 'L', $ipoint;
            for my $point (@{$line}) {
                my ($x, $y) = @{$point}[0..1];
                $rpoint .= pack 'dd', $x,$y;
                $ipoint ++;
                $xmin = $x      if $x<$xmin;
                $ymin = $y      if $y<$ymin;
                $xmax = $x      if $x>$xmax;
                $ymax = $y      if $y>$ymax;
            }
        }
        $rdata = pack( 'LddddLL', $self->{TYPE}, $xmin,$ymin,$xmax,$ymax, scalar(@{$data}), $ipoint ) . $rpart . $rpoint;
    }

    $self->{RCOUNT} ++;

    my $shx = $self->{SHX};
    print $shx pack 'NN', $self->{SHPSIZE}, length($rdata)/2;
    $self->{SHXSIZE} += 4;

    my $shp = $self->{SHP};
    print $shp pack 'NN', $self->{RCOUNT}, length($rdata)/2;
    print $shp $rdata;

    $self->{SHPSIZE} += 4+length($rdata)/2;
    $self->{XMIN} = $xmin       if  $xmin < $self->{XMIN} ;
    $self->{YMIN} = $ymin       if  $ymin < $self->{YMIN} ;
    $self->{XMAX} = $xmax       if  $xmax > $self->{XMAX} ;
    $self->{YMAX} = $ymax       if  $ymax > $self->{YMAX} ;

    $self->{DBF}->appendrow_arrayref( \@_ );
}





sub close {
    my $self = shift;

    my $shp = $self->{SHP};
    seek $shp, 0, 0;
    print $shp pack("Nx[20]NLLddddx[32]",( 9994,$self->{SHPSIZE},1000,$self->{TYPE},$self->{XMIN},$self->{YMIN},$self->{XMAX},$self->{YMAX} ));
    close $shp;

    my $shx = $self->{SHX};
    seek $shx, 0, 0;
    print $shx pack("Nx[20]NLLddddx[32]",( 9994,$self->{SHXSIZE},1000,$self->{TYPE},$self->{XMIN},$self->{YMIN},$self->{XMAX},$self->{YMAX} ));
    close $shx;

    $self->{DBF}->closeDB();
}

1;
