(function(window, document, Date){
    "use strict";
    var ready = ["complete", "interactive"].indexOf(document.readyState) != -1,
        mel   ,
        ns    = "http://www.w3.org/1998/Math/MathML",
        div   ,
        mfrac ,
        script,
        age   ,
        ael   ,
        rcb   = function (e){ready = true; cb();},
        cb    = function(){
            /* EMail */
            mel = document.getElementById('email-link');
            if(mel) mel.href = '\x6d\x61\x69\x6cto:\x77\x65\x62\x40kazlauskas.me';

            /* MathJax */
            if(!! document.getElementsByTagName('math').length
               && document.createElementNS){
                div     = document.createElement("div");
                mfrac   = div.appendChild(document.createElementNS(ns,"math"))
                             .appendChild(document.createElementNS(ns,"mfrac"));
                mfrac.appendChild(document.createElementNS(ns,"mi"))
                     .appendChild(document.createTextNode("xx"));
                mfrac.appendChild(document.createElementNS(ns,"mi"))
                     .appendChild(document.createTextNode("yy"));

                div.style.position = "absolute";
                document.body.appendChild(div);
                if(div.offsetHeight < div.offsetWidth){
                    script       = document.createElement("script");
                    script.async = true;
                    script.type  = "text/javascript";
                    script.src   = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML";
                    document.head.appendChild(script);
                }
                document.body.removeChild(div);
            }

            /* Age */
            age = (~~(new Date / 864E5) - 9200) / 36.525;
            ael = document.getElementById('age');
            if(ael) ael.innerText = ael.textContent =
                ~~age % 10 ? ~~age / 10 : ~~(age / 10);
        };

    if(ready) rcb();
    (window.addEventListener || window.attachEvent)('load', rcb);
})(window, document, Date);
