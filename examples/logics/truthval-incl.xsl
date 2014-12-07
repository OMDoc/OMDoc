<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=truthval-omdoc.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#truthval"/><catalogue for="#truthval-nd"><loc theory="truthval" omdoc="truthval.omdoc#truthval"/><loc theory="inference-rules" omdoc="inference-rules.omdoc#inference-rules"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="truthval-tmpl.xsl"/><xsl:include href="truthval-tmpl.xsl"/><xsl:include href="inference-rules-tmpl.xsl"/></xsl:stylesheet>
