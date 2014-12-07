<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:omdoc="http://omdoc.org/ns"
  xmlns:dc="http://purl.org/dc/elements/1.1/" 
  xmlns:ltx="http://dlmf.nist.gov/LaTeXML">

  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="*">
    <xsl:message>Cannot deal with element <xsl:value-of select="local-name()"/> yet!</xsl:message>
  </xsl:template>

  <xsl:template match="/">
    <xsl:comment>Generated from LaTeXML, do not edit!</xsl:comment>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="ltx:document">
    <xsl:variable name="meta" select="ltx:title|ltx:creator|ltx:date"/>
    <omdoc:omdoc version="1.6"> 
      <xsl:if test="$meta"><omdoc:metadata><xsl:apply-templates select="$meta"/></omdoc:metadata></xsl:if>
      <xsl:apply-templates select="*[not(local-name()='title' or local-name()='creator' or local-name()='date')]"/>
    </omdoc:omdoc>
  </xsl:template>

  <xsl:template match="ltx:title">
    <dc:title><xsl:apply-templates/></dc:title>
  </xsl:template>

  <xsl:template match="ltx:text|ltx:personname">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="ltx:creator">
    <dc:creator><xsl:apply-templates/></dc:creator>
  </xsl:template>

  <xsl:template match="ltx:date">
    <dc:date><xsl:apply-templates/></dc:date>
  </xsl:template>

  <xsl:template match="ltx:part|ltx:chapter|ltx:section|ltx:subsection">
    <xsl:variable name="meta" select="ltx:title"/>
    <omdoc:omdoc type='sectioning'>
      <xsl:if test="@id"><xsl:attribute name="xml:id"><xsl:value-of select="@id"/></xsl:attribute></xsl:if>
      <xsl:if test="$meta!=''"><omdoc:metadata><xsl:apply-templates select="$meta"/></omdoc:metadata></xsl:if>
      <xsl:apply-templates select="*[local-name()!='title']"/>
    </omdoc:omdoc>
  </xsl:template>

  <xsl:template match="ltx:paragraph|ltx:para">
    <omdoc:omtext>
      <xsl:if test="@id"><xsl:attribute name="xml:id"><xsl:value-of select="@id"/></xsl:attribute></xsl:if>
      <omdoc:CMP><xsl:apply-templates/></omdoc:CMP>
    </omdoc:omtext>
  </xsl:template>

  <xsl:template match="ltx:p">
    <omdoc:p><xsl:apply-templates/></omdoc:p>
  </xsl:template>

  <xsl:template match="ltx:paragraph/ltx:para">
    <omdoc:p>
      <xsl:if test="@id"><xsl:attribute name="xml:id"><xsl:value-of select="@id"/></xsl:attribute></xsl:if>
      <xsl:apply-templates/>
    </omdoc:p>
  </xsl:template>

  <xsl:template match="ltx:paragraph/ltx:para">
    <omdoc:p>
      <xsl:if test="@id"><xsl:attribute name="xml:id"><xsl:value-of select="@id"/></xsl:attribute></xsl:if>
      <xsl:apply-templates/>
    </omdoc:p>
  </xsl:template>

  <xsl:template match="ltx:text[following-sibling::ltx:indexmark]"/>
  <xsl:template match="ltx:indexmark[preceding-sibling::ltx:text]">
    <omdoc:idx>
      <omdoc:idt><xsl:apply-templates select="preceding-sibling::ltx:text"/></omdoc:idt>
      <omdoc:ide>
        <xsl:apply-templates select="ltx:indexphrase"/>
      </omdoc:ide>
    </omdoc:idx>
  </xsl:template>

  <xsl:template match="ltx:indexphrase">
    <omdoc:idp><xsl:apply-templates/></omdoc:idp>
  </xsl:template>
</xsl:stylesheet>
