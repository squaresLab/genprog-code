---
layout: page
title: "People"
description: "Collaborators working on this project"
header-img: "img/header.jpg"
priority: 1
---

GenProg is primarily a collaboration between three PIs:
* <img src="/genprog-code/img/weimer.jpg" width="100" /> [Westley Weimer](http://www.cs.virginia.edu/~weimer) at the University of Michigan - Ann Arbor
* <img src="/genprog-code/img/steph.jpg" width="100" /> [Stephanie Forrest](http://www.cs.unm.edu/~forrest) at the
University of New Mexico
* <img src="/genprog-code/img/clg.jpg" width="100" /> [Claire Le&nbsp;Goues](http://www.cs.cmu.edu/~clegoues/) at Carnegie Mellon
University. 


In addition, we are indebted to a number of graduate and undergraduate
researchers, without whom this project would not be possible:

---

### Current researchers:

{% for person in site.data.people %} 
{% if person.visible == true %}
{% if person.current == true %}
{% if person.url %}
* **[{% if person.phd %}Dr.{% endif %} {{ person.name }}]({{ person.url }}):** {{ person.affiliation }}
{% else %}
* **{% if person.phd %}Dr.{% endif %} {{ person.name }}:** {{ person.affiliation }}
{% endif %}
{% endif %}
{% endif %}
{% endfor %}

---

### Past researchers:

{% for person in site.data.people %}{% if person.current != true and person.visible == true %} 
* **{% if person.url %}[{% if person.phd %}Dr. {% endif %}{{ person.name }}]({{ person.url }}){% else %}{% if person.phd %}Dr. {% endif %}{{ person.name }}{% endif %}:** {{ person.affiliation }}  {% endif %}  {% endfor %} 
