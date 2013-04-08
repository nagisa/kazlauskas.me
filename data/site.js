(function(window, document, Date){
    "use strict";
    (window.addEventListener || window.attachEvent)('load', function(){
        /* EMail */
        var el = document.getElementById('email');
        if(el) el.href = '\x6d\x61\x69\x6cto:\x77\x65\x62\x40kazlauskas.me';

        /* MathJax */
        if(!! document.getElementsByTagName('math').length
           && document.createElementNS){
            var hasMath = false;
            var ns      = "http://www.w3.org/1998/Math/MathML";
            var div     = document.createElement("div");
            var mfrac   = div.appendChild(document.createElementNS(ns,"math"))
                             .appendChild(document.createElementNS(ns,"mfrac"));

            mfrac.appendChild(document.createElementNS(ns,"mi"))
                 .appendChild(document.createTextNode("xx"));
            mfrac.appendChild(document.createElementNS(ns,"mi"))
                 .appendChild(document.createTextNode("yy"));

            div.style.position = "absolute";
            document.body.appendChild(div);
            hasMath = div.offsetHeight > div.offsetWidth;
            document.body.removeChild(div);
            if(!hasMath){
                var script   = document.createElement("script");
                script.async = true;
                script.type  = "text/javascript";
                script.src   = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML";
                document.head.appendChild(script);
            }
        }

        /* Age */
        var age = (~~(new Date / 864E5) - 9200) / 36.525;
        var ael = document.getElementById('age');
        if(ael) ael.innerText = ael.textContent =
            ~~age % 10 ? ~~age / 10 : ~~(age / 10);
    });
})(window, document, Date);
