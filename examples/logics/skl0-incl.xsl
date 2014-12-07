<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=skl0-omdoc.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#skl0"><loc theory="truthval" omdoc="truthval.omdoc#truthval"/><loc theory="pl0" omdoc="pl0.omdoc#pl0"/><loc theory="truth-tables" omdoc="truthtables.omdoc#truth-tables"/><loc theory="simpletypes" omdoc="simpletypes.omdoc#simpletypes"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="skl0-tmpl.xsl"/><xsl:include href="truthval-tmpl.xsl"/><xsl:include href="pl0-tmpl.xsl"/><xsl:include href="truthtables-tmpl.xsl"/><xsl:include href="simpletypes-tmpl.xsl"/></xsl:stylesheet>
