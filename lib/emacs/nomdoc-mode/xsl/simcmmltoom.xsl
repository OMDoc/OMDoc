<?xml version="1.0" encoding="utf-8"?>
<!-- ********************************************************** -->
<!-- XSL Transform of OpenMath to Simplified Content MathML     -->
<!--                                                            -->
<!-- Author: Clare M. So <clare@scl.csd.uwo.ca>                 -->
<!--                                                            -->
<!-- September 2002                                             -->
<!-- ********************************************************** -->


<!-- Special MathML Entities -->

<!DOCTYPE stylesheet [
<!ENTITY pi "&#x003C0;">
<!ENTITY e "&#x02147E;">
<!ENTITY ee "&#x02147E;">
<!ENTITY ExponentialE "&#x02147E;">
<!ENTITY ImaginaryI "&#x02148;">
<!ENTITY ii "&#x02148;">
<!ENTITY gamma "&#x003B3;">
<!ENTITY infin "&#x0221E;">
<!ENTITY infty "&#x0221E;">
<!ENTITY true "&#xF0002;">
<!ENTITY false "&#xF0003;">
<!ENTITY NotANumber "&#xF0001;">
<!ENTITY NaN "&#xF0001;">
]>


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:cmml="http://www.w3.org/1998/Math/MathML"
  xmlns="http://www.openmath.org/OpenMath"
  exclude-result-prefixes="cmml"
  version="1.0">

  <xsl:output method="xml" indent="yes"/>
  
  <xsl:strip-space elements="*"/>


   <!-- apply -->
   <xsl:template match="cmml:apply">
     <xsl:choose>
       <xsl:when test="contains(*[1]/@definitionURL,'quant1') or contains(*[1]/@definitionURL,'lambda')">
         <OMBIND>
           <xsl:choose>
             <xsl:when test="cmml:bvar">
               <xsl:apply-templates select="*[1]"/>
               <OMBVAR>
                 <xsl:for-each select="cmml:bvar">
                   <xsl:apply-templates select="*[1]"/>
                 </xsl:for-each>
               </OMBVAR>
               <xsl:apply-templates select="*[last()]"/>
             </xsl:when>
             <xsl:otherwise>
               <xsl:apply-templates/>
             </xsl:otherwise>
           </xsl:choose>
         </OMBIND>
       </xsl:when>
       <xsl:otherwise>
         <OMA>
           <xsl:choose>
             <xsl:when test="cmml:bvar">
               <xsl:apply-templates select="*[1]"/>
               <OMBVAR>
                 <xsl:for-each select="cmml:bvar">
                   <xsl:apply-templates select="*[1]"/>
                 </xsl:for-each>
               </OMBVAR>
               <xsl:apply-templates select="*[last()]"/>
             </xsl:when>
             <xsl:otherwise>
               <xsl:apply-templates/>
             </xsl:otherwise>
           </xsl:choose>
         </OMA>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>
         


   <!-- ci -->
   <xsl:template match="cmml:ci">
     <OMV>
       <xsl:attribute name="name">
         <xsl:value-of select="."/>
       </xsl:attribute>
     </OMV>
   </xsl:template>


   <xsl:template match="cmml:ci[@type]">
     <OMATTR>
       <OMATP>
         <OMS cd='mathmltypes' name='type'/>
         <OMS cd='mathmltypes'>
           <xsl:attribute name='name'>
             <xsl:value-of select="concat(normalize-space(@type),'_type')"/>
           </xsl:attribute>
         </OMS>
       </OMATP>
       <OMV>
         <xsl:attribute name="name">
           <xsl:value-of select="normalize-space(.)"/>
         </xsl:attribute>
       </OMV>
     </OMATTR>
   </xsl:template>


   <!-- cn -->
   <xsl:template match="cmml:cn[@type='integer']">
     <OMI>
       <xsl:value-of select="normalize-space(.)"/>
     </OMI>
   </xsl:template>


   <xsl:template match="cmml:cn[@type='integer' and @base='16']">
     <xsl:variable name="num" select="normalize-space(.)"/>
     <OMI>
       <xsl:choose>
         <xsl:when test="contains($num,'-')">
           <xsl:value-of select="concat('-',concat('x',substring-after($num,'-')))"/>
         </xsl:when>
         <xsl:otherwise>
           <xsl:value-of select="concat('x',$num)"/>
         </xsl:otherwise>
       </xsl:choose>
     </OMI>
   </xsl:template>


   <xsl:template match="cmml:cn">
     <OMF>
       <xsl:attribute name="dec">
         <xsl:value-of select="."/>
       </xsl:attribute>
     </OMF>
   </xsl:template>


   <xsl:template match="cmml:cn[@base='16']">
     <xsl:variable name="num" select="normalize-space(.)"/>
     <OMF>
       <xsl:attribute name='hex'>
         <xsl:choose>
           <xsl:when test="contains($num,'-')">
             <xsl:value-of select="concat('-',concat('x',substring-after($num,'-')))"/>
           </xsl:when>
           <xsl:otherwise>
             <xsl:value-of select="concat('x',$num)"/>
           </xsl:otherwise>
         </xsl:choose>
       </xsl:attribute>
     </OMF>
   </xsl:template>


   <!-- semantics -->
   <xsl:template match="cmml:semantics">
     <OMATTR>
       <OMATP>
         <xsl:apply-templates select="*[position()!=last()]"/>
       </OMATP>
       <xsl:apply-templates select="*[position()=last()]"/>
     </OMATTR>
   </xsl:template>


   <!-- mtext -->
   <xsl:template match="cmml:mtext">
     <OMSTR>
       <xsl:value-of select="normalize-space(.)"/>
     </OMSTR>
   </xsl:template>


   <!-- csymbol -->
   <xsl:template match="cmml:csymbol">
     <OMS>
       <xsl:attribute name="cd">
         <xsl:value-of select="substring-before(substring-after(@definitionURL,'cd/'),'#')"/>
       </xsl:attribute>
       <xsl:attribute name="name">
         <xsl:value-of select="substring-after(@definitionURL,'#')"/>
       </xsl:attribute>
     </OMS>
   </xsl:template>

</xsl:stylesheet>
