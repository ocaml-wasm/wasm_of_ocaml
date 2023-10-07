var a = new Uint8Array(65536)
a.fill(97)
a[0]=195
a[1]=169
a[0]=229
a[1]=133
a[2]=182
/*
*/
const decoder = new TextDecoder('utf-8', {ignoreBOM: 1});
const encoder = new TextEncoder;

var s = decoder.decode(a,{stream:true})

var t= Date.now()
for (var i = 0; i < 150000; i++) encoder.encodeInto(s.slice(0), a)
console.log(Date.now() - t);

/*
Node
   ASCII only:       0.35s
   Latin1:           1.8s
   Unicode:          4.3s
Chrome
   ASCII/Latin 1:    5.5s
   Unicode:         17s

===

stringref
            node     chrome
   ASCII:    8s       20s
   Latin1:   8s       20s
   Unicode: 14s       20s

buffer
             node   chrome
   ASCII:    7s       13s
   Latin1:   8s       13s
   Unicode: 11s       24s
*/
