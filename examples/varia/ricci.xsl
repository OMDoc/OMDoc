<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns:omdoc="http://www.mathweb.org/omdoc"
  xmlns:exsl="http://exslt.org/common" 
                version="1.0">

  <xsl:output method="xml"/>

  <xsl:template match="/">
    <xsl:apply-templates select="id('in-tworicci')" mode="transport">
      <xsl:with-param name="morphism" select="//*[@id='the-morphism']"/>
    </xsl:apply-templates>
  </xsl:template>

<xsl:template match="*">
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates/>
  </xsl:copy>
</xsl:template>

<xsl:template match="om:OMA[om:OMS[position()=1 and @cd='arith1' and @name='times'] and 
                            count(*)=3 and not(*[position() &gt; 1 and local-name()!='OMF'])]">
  <om:OMF dec="{om:OMF[1]/@dec * om:OMF[2]/@dec}"/>
</xsl:template>

<xsl:template match="*" mode="transport">
  <xsl:param name="morphism"/>
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates mode="transport">
      <xsl:with-param name="morphism" select="$morphism"/>
    </xsl:apply-templates>
  </xsl:copy>
</xsl:template>

<xsl:template match="om:OMS" mode="transport">
  <xsl:param name="morphism"/>
  <xsl:variable name="cd" select="@cd"/>
  <xsl:variable name="name" select="@name"/>
  <xsl:variable name="value" 
    select="$morphism/omdoc:requation[omdoc:pattern/om:OMOBJ/om:OMS[@cd=$cd and @name=$name]]/omdoc:pattern/om:OMOBJ"/>
  <xsl:choose>
    <xsl:when test="$value"><xsl:copy-of select="$value"/><xsl:message>replacing</xsl:message></xsl:when>
    <xsl:otherwise><om:OMS cd="{$cd}" name="{$name}"/></xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
