---
layout: page
title: "People"
description: "Collaborators working on this project"
header-img: "img/header.jpg"
priority: 1
---

GenProg is primarily a collaboration
between [Westley Weimer](http://www.cs.virginia.edu/~weimer) at the University
of Michigan - Ann Arbor, [Stephanie Forrest](http://www.cs.unm.edu/~forrest) at the
University of New Mexico,
and [Claire Le&nbsp;Goues](http://www.cs.cmu.edu/~clegoues/) at Carnegie Mellon
University.

In addition, we are indebted to a number of graduate and undergraduate
researchers, without whom this project would not be possible:

---

### Current researchers:

{% for person in site.data.people %} {% if person.visible == true %}
{% if person.current == true %}
{% if person.phd %}
{% if person.url %}
**[Dr. {{ person.name }}]({{ person.url }}):** {{ person.affiliation }}
{% else %}
**Dr. {{ person.name }}:** {{ person.affiliation }}
{% endif %}{% endif %}{% endif %}{% endif %}{% endfor %}

{% for person in site.data.people %} {% if person.visible == true %}
{% if person.current == true %}
{% if person.phd != true %}
{% if person.url %}
**[{{ person.name }}]({{ person.url }}):** {{ person.affiliation }}
{% else %}
**{{ person.name }}:** {{ person.affiliation }}
{% endif %}{% endif %}{% endif %}{% endif %}{% endfor %}

---

### Past researchers:

{% for person in site.data.people %} {% if person.visible == true %}
{% if person.current != true %}
{% if person.phd %}
{% if person.url %}
**[Dr. {{ person.name }}]({{ person.url }}):** {{ person.affiliation }}
{% else %}
**Dr. {{ person.name }}:** {{ person.affiliation }}
{% endif %}{% endif %}{% endif %}{% endif %}{% endfor %}

{% for person in site.data.people %} {% if person.visible == true %}
{% if person.current != true %}
{% if person.phd != true %}
{% if person.url %}
**[{{ person.name }}]({{ person.url }}):** {{ person.affiliation }}
{% else %}
**{{ person.name }}:** {{ person.affiliation }}
{% endif %}{% endif %}{% endif %}{% endif %}{% endfor %}
