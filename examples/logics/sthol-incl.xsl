<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=sthol-omdoc.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#sthol"><loc theory="pl0" omdoc="pl0.omdoc#pl0"/><loc theory="lambda-calc" omdoc="lambda-calc.omdoc#lambda-calc"/><loc theory="ind" omdoc="ind.omdoc#ind"/></catalogue><catalogue for="#stholif"><loc theory="sthol" omdoc="sthol.omdoc#sthol"/><loc theory="pl0" omdoc="pl0.omdoc#pl0"/></catalogue><catalogue for="#sthold"><loc theory="stholif" omdoc="#stholif"/><loc theory="ind" omdoc="ind.omdoc#ind"/></catalogue><catalogue for="#stholc"><loc theory="sthold" omdoc="#sthold"/><loc theory="pl0" omdoc="pl0.omdoc#pl0"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="sthol-tmpl.xsl"/><xsl:include href="pl0-tmpl.xsl"/><xsl:include href="lambda-calc-tmpl.xsl"/><xsl:include href="ind-tmpl.xsl"/><xsl:include href="sthol-tmpl.xsl"/></xsl:stylesheet>
