<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=arith1-omdoc.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#arith1"><loc theory="sts" omdoc="../omstd/sts.omdoc#sts"/><loc theory="relation1" omdoc="../omstd/relation1.omdoc#relation1"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="presex-tmpl.xsl"/><xsl:include href="../omstd/sts-tmpl.xsl"/><xsl:include href="../omstd/relation1-tmpl.xsl"/></xsl:stylesheet>
