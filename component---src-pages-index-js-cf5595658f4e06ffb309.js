(self.webpackChunkgatsby_starter_blog=self.webpackChunkgatsby_starter_blog||[]).push([[678],{7361:function(e){"use strict";e.exports=JSON.parse('{"layout":"fixed","backgroundColor":"#282828","images":{"fallback":{"src":"/static/7b5d665746a4df38fcb90999d7de43bb/d24ee/profile-pic.jpg","srcSet":"/static/7b5d665746a4df38fcb90999d7de43bb/d24ee/profile-pic.jpg 50w,\\n/static/7b5d665746a4df38fcb90999d7de43bb/64618/profile-pic.jpg 100w","sizes":"50px"},"sources":[{"srcSet":"/static/7b5d665746a4df38fcb90999d7de43bb/d4bf4/profile-pic.avif 50w,\\n/static/7b5d665746a4df38fcb90999d7de43bb/ee81f/profile-pic.avif 100w","type":"image/avif","sizes":"50px"},{"srcSet":"/static/7b5d665746a4df38fcb90999d7de43bb/3faea/profile-pic.webp 50w,\\n/static/7b5d665746a4df38fcb90999d7de43bb/6a679/profile-pic.webp 100w","type":"image/webp","sizes":"50px"}]},"width":50,"height":50}')},9535:function(e,t,l){"use strict";var a=l(7294),i=l(5444),r=l(410);t.Z=function(){var e,t,n=(0,i.useStaticQuery)("3257411868"),s=null===(e=n.site.siteMetadata)||void 0===e?void 0:e.author,c=null===(t=n.site.siteMetadata)||void 0===t?void 0:t.social;return a.createElement("div",{className:"bio"},a.createElement(r.S,{className:"bio-avatar",layout:"fixed",formats:["AUTO","WEBP","AVIF"],src:"../images/profile-pic.png",width:50,height:50,quality:95,alt:"Profile picture",__imageData:l(7361)}),(null==s?void 0:s.name)&&a.createElement("p",null,"Written by ",a.createElement("strong",null,s.name)," ",(null==s?void 0:s.summary)||null," ",a.createElement("a",{href:"https://twitter.com/"+((null==c?void 0:c.twitter)||"")},"You can follow him on Twitter")))}},7704:function(e,t,l){"use strict";l.r(t);var a=l(7294),i=l(5444),r=l(9535),n=l(7198),s=l(3751);t.default=function(e){var t,l=e.data,c=e.location,o=(null===(t=l.site.siteMetadata)||void 0===t?void 0:t.title)||"Title",d=l.allMarkdownRemark.nodes;return 0===d.length?a.createElement(n.Z,{location:c,title:o},a.createElement(s.Z,{title:"All posts"}),a.createElement(r.Z,null),a.createElement("p",null,'No blog posts found. Add markdown posts to "content/blog" (or the directory you specified for the "gatsby-source-filesystem" plugin in gatsby-config.js).')):a.createElement(n.Z,{location:c,title:o},a.createElement(s.Z,{title:"All posts"}),a.createElement(r.Z,null),a.createElement("ol",{style:{listStyle:"none"}},d.map((function(e){var t=e.frontmatter.title||e.fields.slug;return a.createElement("li",{key:e.fields.slug},a.createElement("article",{className:"post-list-item",itemScope:!0,itemType:"http://schema.org/Article"},a.createElement("header",null,a.createElement("h2",null,a.createElement(i.Link,{to:e.fields.slug,itemProp:"url"},a.createElement("span",{itemProp:"headline"},t))),a.createElement("small",null,e.frontmatter.date)),a.createElement("section",null,a.createElement("p",{dangerouslySetInnerHTML:{__html:e.frontmatter.description||e.excerpt},itemProp:"description"}))))}))))}}}]);
//# sourceMappingURL=component---src-pages-index-js-cf5595658f4e06ffb309.js.map