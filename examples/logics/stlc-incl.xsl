<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=stlc.omdoc.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#stlc"><loc theory="simpletypes" omdoc="simpletypes.omdoc#simpletypes"/><loc theory="ind" omdoc="ind.omdoc#ind"/><loc theory="lambda-calc" omdoc="lambda-calc.omdoc#lambda-calc"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="stlc-tmpl.xsl"/><xsl:include href="simpletypes-tmpl.xsl"/><xsl:include href="ind-tmpl.xsl"/><xsl:include href="lambda-calc-tmpl.xsl"/></xsl:stylesheet>
