#/usr/bin/perl

open(OUT,">library.r");

foreach $file (<EuData/R/*>){
    print "... reading $file\n";

    open(IN,$file);

    print OUT "\n# copied from $file\n\n";

    for (<IN>){
       print OUT $_;	
    }    

    close(IN);
    
}

close(OUT);
