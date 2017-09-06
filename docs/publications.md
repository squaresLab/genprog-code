---
layout: page
title: "Publications"
description: "GenProg-related publications"
header-img: "img/header.jpg"
---

This page focusing primarily on publications whose data and experiments were
implemented on top of the GenProg codebase by researchers related to the core
GenProg team.  [program-repair.org](http://program-repair.org/) provides a much
more complete, community-supported list of papers related to program repair
generally, including papers that build upon or compare to GenProg.

*This page is sorted topically.*


---

Broad overviews and high-level arguments
-------------------------


{% for pub in site.data.pubs %} {% if pub.category contains "broad" %}
{{ pub.entry }}  
[PDF](https://squareslab.github.io/papers-repo/pdfs/{{ pub.pdf }})
[DOI]({{ pub.doi }})
[BibTex](https://squareslab.github.io/papers-repo/bib/{{ pub.bibtex }}) 
{% endif %} {% endfor %}



---

Automated Program Repair
-------------------------

### ...targeting C programs

{% for pub in site.data.pubs %} {% if pub.category contains "targetC" %}
{% if pub.aid %} <a id="{{ pub.aid }}"></a>{% endif %}
{{ pub.entry }} {% if pub.note %}**Note: {{ pub.note }}**{% endif %}  
[PDF](https://squareslab.github.io/papers-repo/pdfs/{{ pub.pdf }})
[DOI]({{ pub.doi }})
[BibTex](https://squareslab.github.io/papers-repo/bib/{{ pub.bibtex }}) 
{% if pub.site or pub.code or pub.dataset or pub.results or pub.vm %} |
{% if pub.site %} [Code, Dataset, and Results]({{ pub.site }}) {% endif %} {% if pub.code %}  [Code]({{ pub.code }}) {% endif %}  {% if pub.vm %} [Virtual Machine Images]({{ pub.vm }})
{% endif %} {% if pub.dataset %} [Dataset]({{ pub.dataset }}) {% endif %} {% if pub.results %}  [Results]({{ pub.results }}) {% endif %}
{% endif %} 
{% endif %}
{% endfor %}

### ...targeting assembly code


{% for pub in site.data.pubs %} {% if pub.category contains "asm" %}
{% if pub.aid %} <a id="{{ pub.aid }}"></a>{% endif %}
{{ pub.entry }} {% if pub.note %}**Note: {{ pub.note }}**{% endif %}  
[PDF](https://squareslab.github.io/papers-repo/pdfs/{{ pub.pdf }})
[DOI]({{ pub.doi }})
[BibTex](https://squareslab.github.io/papers-repo/bib/{{ pub.bibtex }}) 
{% if pub.site or pub.code or pub.dataset or pub.results or pub.vm %} |
{% if pub.site %} [Code, Dataset, and Results]({{ pub.site }}) {% endif %} {% if pub.code %}  [Code]({{ pub.code }}) {% endif %}  {% if pub.vm %} [Virtual Machine Images]({{ pub.vm }})
{% endif %} {% if pub.dataset %} [Dataset]({{ pub.dataset }}) {% endif %} {% if pub.results %}  [Results]({{ pub.results }}) {% endif %}
{% endif %} 
{% endif %}
{% endfor %}

---

Search specifics
-----------------

{% for pub in site.data.pubs %} {% if pub.category contains "search" %}
{% if pub.aid %} <a id="{{ pub.aid }}"></a>{% endif %}
{{ pub.entry }} {% if pub.note %}**Note: {{ pub.note }}**{% endif %}  
[PDF](https://squareslab.github.io/papers-repo/pdfs/{{ pub.pdf }})
[DOI]({{ pub.doi }})
[BibTex](https://squareslab.github.io/papers-repo/bib/{{ pub.bibtex }}) 
{% if pub.site or pub.code or pub.dataset or pub.results or pub.vm %} |
{% if pub.site %} [Code, Dataset, and Results]({{ pub.site }}) {% endif %} {% if pub.code %}  [Code]({{ pub.code }}) {% endif %}  {% if pub.vm %} [Virtual Machine Images]({{ pub.vm }})
{% endif %} {% if pub.dataset %} [Dataset]({{ pub.dataset }}) {% endif %} {% if pub.results %}  [Results]({{ pub.results }}) {% endif %}
{% endif %} 
{% endif %}
{% endfor %}

---

Patch quality, software robustness
-----------------------------------

{% for pub in site.data.pubs %} {% if pub.category contains "robustness" %}
{% if pub.aid %} <a id="{{ pub.aid }}"></a>{% endif %}
{{ pub.entry }} {% if pub.note %}**Note: {{ pub.note }}**{% endif %}  
[PDF](https://squareslab.github.io/papers-repo/pdfs/{{ pub.pdf }})
[DOI]({{ pub.doi }})
[BibTex](https://squareslab.github.io/papers-repo/bib/{{ pub.bibtex }}) 
{% if pub.site or pub.code or pub.dataset or pub.results or pub.vm %} |
{% if pub.site %} [Code, Dataset, and Results]({{ pub.site }}) {% endif %} {% if pub.code %}  [Code]({{ pub.code }}) {% endif %}  {% if pub.vm %} [Virtual Machine Images]({{ pub.vm }})
{% endif %} {% if pub.dataset %} [Dataset]({{ pub.dataset }}) {% endif %} {% if pub.results %}  [Results]({{ pub.results }}) {% endif %}
{% endif %} 
{% endif %}
{% endfor %}

---

Non-functional or quality properties:
-------------------------------------

{% for pub in site.data.pubs %} {% if pub.category contains "qprop" %}
{% if pub.aid %} <a id="{{ pub.aid }}"></a>{% endif %}
{{ pub.entry }} {% if pub.note %}**Note: {{ pub.note }}**{% endif %}  
[PDF](https://squareslab.github.io/papers-repo/pdfs/{{ pub.pdf }})
[DOI]({{ pub.doi }})
[BibTex](https://squareslab.github.io/papers-repo/bib/{{ pub.bibtex }}) 
{% if pub.appendix %} [Appendix]({{ pub.appendix }}) {% endif %}
{% if pub.site or pub.code or pub.dataset or pub.results or pub.vm %} |
{% if pub.site %} [Code, Dataset, and Results]({{ pub.site }}) {% endif %} {% if pub.code %}  [Code]({{ pub.code }}) {% endif %}  {% if pub.vm %} [Virtual Machine Images]({{ pub.vm }})
{% endif %} {% if pub.dataset %} [Dataset]({{ pub.dataset }}) {% endif %} {% if pub.results %}  [Results]({{ pub.results }}) {% endif %}
{% endif %} 
{% endif %}
{% endfor %}
