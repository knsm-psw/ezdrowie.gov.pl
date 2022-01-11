#!/usr/bin/perl
$dat = $ARGV[0];
$thisY = 5;
$prevY = 9;
$r=0;
##
$dat =~ s/.csv//;
%MC = (
  'styczeń' => '01',
  'luty' => '02',
  'marzec' => '03',
  'kwiecień' => '04',
  'maj' => '05',
  'czerwiec' => '06',
  'lipiec' => '07',
  'sierpień' => '08',
  'wrzesień' => '09',
  'październik' => '10',
  'listopad' => '11',
  'grudzień' => '12', );
##
($ddy, $ddm) = split /_/, $dat;
$ddm = $MC{$ddm};
$dddd = "$ddy-$ddm";

while (<>) {
 @tmp = split /;/, $_;
 $co = $tmp[1];
 $woj = $tmp[2];
 if ($r <3 ) { print STDERR "### $co;$tmp[$thisY];$tmp[$prevY]\n" }

 $K{$co}{$woj} +=  $tmp[$thisY];
 $P{$co}{$woj} += $tmp[$prevY];
 ### Razem na PL w dziedzinie co
 $K{$co}{'PL'} += $tmp[$thisY];
 $P{$co}{'PL'} += $tmp[$prevY];
 ### poniższe policzysz
 ### Razem na wojewodztwo
 ##$K{'TOTAL'}{$woj} += $tmp[$thisY];
 ##$P{'TOTAL'}{$woj} += $tmp[$prevY];
 ## niepotrzebne
 ##$K{'TOTAL'}{'PL'} += $tmp[$thisY];
 ##$P{'TOTAL'}{'PL'} += $tmp[$prevY];

 $r++;
}


for $k (sort keys %K) {
   for $w (sort keys %{$K {$k} }) {
     unless ($k eq '' || $k =~ /Rozdział/ || $k =~ /Tabela/i ) {
       print "$dddd;$k;$w;$K{$k}{$w};$P{$k}{$w}\n"
     }
   }
}
