<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=abs.omdoc.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#simple-abs"><loc theory="arith1" omdoc="../omstd/arith1.omdoc#arith1"/><loc theory="relation1" omdoc="../omstd/relation1.omdoc#relation1"/><loc theory="pl1" omdoc="../logics/pl1.omdoc#pl1"/><loc theory="pl0" omdoc="../logics/pl0.omdoc#pl0"/><loc theory="truthval" omdoc="../logics/truthval.omdoc#truthval"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="abs-tmpl.xsl"/><xsl:include href="../omstd/arith1-tmpl.xsl"/><xsl:include href="../omstd/relation1-tmpl.xsl"/><xsl:include href="../logics/pl1-tmpl.xsl"/><xsl:include href="../logics/pl0-tmpl.xsl"/><xsl:include href="../logics/truthval-tmpl.xsl"/></xsl:stylesheet>
