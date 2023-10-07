var a = new Uint8Array(65536)
a.fill(97)
a[0]=195
a[1]=169
/*
*/
/*
a[0]=229
a[1]=133
a[2]=182
*/
const decoder = new TextDecoder('utf-8', {ignoreBOM: 1});
var t= Date.now()
for (var i = 0; i < 150000; i++) decoder.decode(a,{stream:true})
console.log(Date.now() - t);

/*
Node
                stream:false   stream:true
   ASCII only:       1s            5.5s
   Latin1:          17s            5.5s
   Unicode:         28s           14.5s

Chrome

   ASCII/Latin 1: 6.8s
   Unicode:       8.7s

===
64k

stringref
            node     chrome
   ASCII:    0.7s     1.2s
   Latin1:  17s       7s
   Unicode: 25s      15s

buffer
             node       chrome
   ASCII:    4s (9s)     10s
   Latin1:  20s (9s)     13s
   Unicode: 28s (16s)    21s

===
128k

stringref
            node     chrome
   ASCII:    9s       11s
   Latin1:  43s       25s
   Unicode: 48s       30s

buffer
             node       chrome
   ASCII:    19s         23s
   Latin1:   19s         23s
   Unicode:  27s         34s
*/
