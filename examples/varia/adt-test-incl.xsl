<?xml version="1.0" standalone="yes"?>
<!--
    an xsl style sheet for presenting openmath symbols used in the 
    OMDoc document with id=VeriFun3.0-2005-03-11.
  
    This xsl style file is automatically generated, do not edit!
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common" version="1.0" exsl:dummy="to get the namespace right" extension-element-prefixes="exsl"><xsl:variable name="tree"><catalogue for="#VeriFun"><loc theory="vafp" omdoc="../logics/vafp.omdoc#vafp"/><loc theory="pl1" omdoc="../logics/pl1.omdoc#pl1"/><loc theory="simpletypes" omdoc="../logics/simpletypes.omdoc#simpletypes"/></catalogue></xsl:variable>

<xsl:variable name="href-cat" select="exsl:node-set($tree)"/>

<xsl:include href="adt-test-tmpl.xsl"/><xsl:include href="../logics/vafp-tmpl.xsl"/><xsl:include href="../logics/pl1-tmpl.xsl"/><xsl:include href="../logics/simpletypes-tmpl.xsl"/></xsl:stylesheet>
