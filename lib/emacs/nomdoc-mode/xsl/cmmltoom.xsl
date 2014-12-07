<?xml version="1.0" encoding="utf-8"?>

<!-- ********************************************************** -->
<!-- XSL Transform of Content MathML to OpenMath                -->
<!--                                                            -->
<!-- Author: Clare M. So <clare@scl.csd.uwo.ca>                 -->
<!--                                                            -->
<!-- July to August 2002                                        -->
<!--                                                            -->
<!-- (Last modified July 9, 2003)                               -->
<!-- ********************************************************** -->



<!-- ********************************************************** -->
<!-- CHANGE LOG                                                 -->
<!-- ********************************************************** -->
<!-- July 9, 2003 - Add templates for diff having degree        -->
<!--                decendent                                   -->




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


  <!-- math -->
  <xsl:template match="cmml:math">
    <OMOBJ>
      <xsl:apply-templates/>
    </OMOBJ>
  </xsl:template>


  <!-- mtext -->
  <xsl:template match="cmml:mtext">
    <OMSTR>
      <xsl:value-of select="normalize-space(.)"/>
    </OMSTR>
  </xsl:template>


  <!-- mtext (with definitionURL) -->
  <xsl:template match="cmml:mtext[@definitionURL]">
    <OMB>
      <xsl:value-of select="normalize-space(.)"/>
    </OMB>
  </xsl:template>

 




  <!-- **************************************************** -->
  <!-- ****************** Token Elements ****************** -->  
  <!-- **************************************************** -->

  <!-- cn, ci, csymbol -->

  <!-- ci -->
  <xsl:template match="cmml:ci">
    <xsl:choose>
      <xsl:when test="starts-with(name(descendant::*),'m')">
        <xsl:comment>ERROR: OpenMath does not support mixed MathML markup</xsl:comment>
      </xsl:when>
      <xsl:otherwise>
        <OMV name="{normalize-space(.)}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- ci (with type) -->
  <xsl:template match="cmml:ci[@type]">
    <OMATTR>
      <OMATP>
        <OMS cd="mathmltypes" name="type"/>
        <OMS cd="mathmltypes" name="{concat(translate(normalize-space(@type),'-','_'),'_type')}"/>
      </OMATP>
      <OMV name="{normalize-space(.)}"/>
     </OMATTR>
  </xsl:template>

  <!-- cn (real floating-point, or constant type) -->
  <xsl:template match="cmml:cn">
    <xsl:variable name="NUM" select="normalize-space(.)"/>
    <xsl:choose>
      <xsl:when test="$NUM='1'">
        <OMS cd="alg1" name="one"/>
      </xsl:when>
      <xsl:when test="$NUM='0'">
        <OMS cd="alg1" name="zero"/>
      </xsl:when>
      <xsl:when test="$NUM='&pi;'">
        <OMS cd="nums1" name="pi"/>
      </xsl:when>
      <xsl:when test="$NUM='&ee;' or $NUM='&ExponentialE;'">
        <OMS cd="nums1" name="e"/>
      </xsl:when>
      <xsl:when test="$NUM='&gamma;'">
        <OMS cd="nums1" name="gamma"/>
      </xsl:when>
      <xsl:when test="$NUM='&infin;' or $NUM='&infty;'">
        <OMS cd="nums1" name="infinity"/>
      </xsl:when>
      <xsl:when test="$NUM='&true;'">
        <OMS cd="logic1" name="true"/>
      </xsl:when>
      <xsl:when test="$NUM='&false;'">
        <OMS cd="logic1" name="false"/>
      </xsl:when>
      <xsl:when test="$NUM='&NotANumber;' or $NUM='&NaN;'">
        <OMS cd="nums1" name="NaN"/>
      </xsl:when>
      <xsl:when test="$NUM='&ii;' or $NUM='&ImaginaryI;'">
        <OMS cd="nums1" name="i"/>
      </xsl:when>
      <xsl:when test="not(contains($NUM,'.'))">
        <OMI><xsl:value-of select="$NUM"/></OMI>
      </xsl:when>
      <xsl:otherwise>
        <OMF dec="{normalize-space(.)}"/>
       </xsl:otherwise>
     </xsl:choose>
  </xsl:template>

  <!-- cn (base 16) -->
  <xsl:template match="cmml:cn[@base='16']">
    <xsl:variable name="NUM" select="normalize-space(.)"/>
    <xsl:choose>
      <xsl:when test="$NUM='1'">
        <OMS cd="alg1" name="one"/>
      </xsl:when>
      <xsl:when test="$NUM='0'">
        <OMS cd="alg1" name="zero"/>
      </xsl:when>
      <xsl:otherwise>
        <OMF hex="{normalize-space(.)}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- cn (type e-notation) -->
  <xsl:template match="cmml:cn[@type='e-notation']">
    <OMA>
      <OMS cd="bigfloat1" name="bigfloat"/>
      <xsl:apply-templates select="text()[1]" mode="convert"/>
      <OMI>10</OMI>
      <xsl:apply-templates select="text()[2]" mode="convert"/>
    </OMA>
  </xsl:template>

  <!-- cn (type integer) -->
  <xsl:template match="cmml:cn[@type='integer']">
    <xsl:variable name="NUM" select="normalize-space(.)"/>
    <xsl:choose>
      <xsl:when test="$NUM='1'">
        <OMS cd="alg1" name="one"/>
      </xsl:when>
      <xsl:when test="$NUM='0'">
        <OMS cd="alg1" name="zero"/>
      </xsl:when>
       <xsl:otherwise>
         <OMI>
           <xsl:value-of select="$NUM"/>
         </OMI>
       </xsl:otherwise>
     </xsl:choose>
  </xsl:template>

  <!-- cn (type integer, base 16) -->
  <xsl:template match="cmml:cn[@type='integer' and @base='16']">
    <xsl:variable name="NUM" select="normalize-space(.)"/>
    <OMI>
      <xsl:choose>
        <xsl:when test="starts-with($NUM,'-')">
          <xsl:value-of select="concat('-x',substring-after($NUM,'-'))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('x',$NUM)"/>
        </xsl:otherwise>
      </xsl:choose>
    </OMI>
  </xsl:template>

  <!-- cn (type integer, base NOT 10 or 16) -->
  <xsl:template match="cmml:cn[@type='integer' and (@base!=16 and @base)]">
    <OMA>
      <OMS cd="nums1" name="based_integer"/>
      <OMI><xsl:value-of select="@base"/></OMI>
      <OMSTR><xsl:value-of select="normalize-space(.)"/></OMSTR>
    </OMA>
  </xsl:template>

  <!-- cn (type rational) -->
  <xsl:template match="cmml:cn[@type='rational']">
    <OMA>
      <OMS cd="nums1" name="rational"/>
      <xsl:apply-templates select="text()[1]" mode="convert"/>
      <xsl:apply-templates select="text()[2]" mode="convert"/>
    </OMA>
  </xsl:template>

  <!-- cn (type complex-cartesian) -->
  <xsl:template match="cmml:cn[@type='complex-cartesian']">
    <OMA>
      <OMS cd="complex1" name="complex_cartesian"/>
      <xsl:apply-templates select="text()[1]" mode="convert"/>
      <xsl:apply-templates select="text()[2]" mode="convert"/>
    </OMA>
  </xsl:template>

  <!-- cn (type complex-polar) -->
  <xsl:template match="cmml:cn[@type='complex-polar']">
    <OMA>
      <OMS cd="complex1" name="complex_polar"/>
      <xsl:apply-templates select="text()[1]" mode="convert"/>
      <xsl:apply-templates select="text()[2]" mode="convert"/>
    </OMA>
  </xsl:template>

  <!-- csymbol -->
  <xsl:template match="cmml:csymbol">
    <xsl:choose>
      <xsl:when test="contains(@definitionURL,'openmath')">
        <OMS>
          <xsl:attribute name="cd">
            <xsl:value-of select="substring-before(substring-after(normalize-space(@definitionURL),'cd/'),'#')"/>
          </xsl:attribute>
          <xsl:attribute name="name">
            <xsl:value-of select="substring-after(normalize-space(@definitionURL),'#')"/>
          </xsl:attribute>
        </OMS>
      </xsl:when>
      <xsl:otherwise>
        <xsl:comment>ERROR: Non OpenMath symbol having the following URL: <xsl:value-of select="@definitionURL"/></xsl:comment>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>





  <!-- **************************************************** -->
  <!-- ************* Basic Content Elements *************** -->  
  <!-- **************************************************** -->

  <!-- apply, reln, fn, interval, inverse, (condition),
       declare, lambda, compose, ident, domain, codomain,
       image, domainofapplication, piecewise, piece, otherwise -->

   <!-- apply or reln (for backwards compatibility) -->
   <xsl:template match="cmml:apply|cmml:reln">
     <OMA>
       <xsl:apply-templates/>
     </OMA>
   </xsl:template>

   <!-- fn (for backwards compatibility) -->
   <xsl:template match="cmml:fn">
     <xsl:apply-templates/>
   </xsl:template>

   <!-- interval -->
   <xsl:template match="cmml:interval">
     <OMA>
       <OMS cd="interval1">
         <xsl:attribute name="name">
           <xsl:choose>
             <xsl:when test="@closure='open'">interval_oo</xsl:when>
             <xsl:when test="@closure='closed'">interval_cc</xsl:when>
             <xsl:when test="@closure='open-closed'">interval_oc</xsl:when>
             <xsl:when test="@closure='closed-open'">interval_co</xsl:when>
             <xsl:when test="*[1][@type='integer'] and *[2][@type='integer']">integer_interval</xsl:when>
             <xsl:otherwise>interval_cc</xsl:otherwise>
           </xsl:choose>
         </xsl:attribute>
       </OMS>    
       <xsl:apply-templates/>
     </OMA>
   </xsl:template>

   <!-- inverse -->
   <xsl:template match="cmml:inverse">
     <OMS cd="fns1" name="inverse"/>
   </xsl:template>

   <!-- condition -->
   <xsl:template match="cmml:condition">
     <xsl:comment>ERROR: The use of "condition" by itself is not supported in OpenMath</xsl:comment>
   </xsl:template>

   <!-- declare (need future improvements) -->
   <xsl:template match="cmml:declare">
     <xsl:if test="*[2]">
       <OMA>
         <OMS cd="relation1" name="eq"/>
         <xsl:apply-templates select="*[1]"/>
         <xsl:apply-templates select="*[2]"/>
       </OMA>
     </xsl:if>
   </xsl:template>

   <!-- lambda -->
   <xsl:template match="cmml:lambda">
     <OMBIND>
       <OMS cd="fns1" name="lambda"/>
       <OMBVAR>
         <xsl:apply-templates select="cmml:bvar[position()>0]/cmml:ci"/>
       </OMBVAR>
       <xsl:apply-templates select="*[last()]"/>
     </OMBIND>
   </xsl:template>

   <!-- compose -->
   <xsl:template match="cmml:compose">
     <OMS cd="fns1" name="left_compose"/>
   </xsl:template>

   <!-- ident -->
   <xsl:template match="cmml:ident">
     <OMS cd="fns1" name="identity"/>
   </xsl:template>

   <!-- domain -->
   <xsl:template match="cmml:domain">
     <OMS cd="fns1" name="domain"/>
   </xsl:template>

   <!-- codomain -->
   <xsl:template match="cmml:codomain">
     <OMS cd="fns1" name="range"/>
   </xsl:template>

   <!-- image -->
   <xsl:template match="cmml:image">
     <OMS cd="fns1" name="image"/>
   </xsl:template>

   <!-- domainofapplication -->
   <xsl:template match="cmml:domainofapplication">
     <OMA>
       <OMS cd="fns1" name="domainofapplication"/>
       <xsl:apply-templates select="*[1]"/>
     </OMA>
   </xsl:template>

   <!-- piecewise, piece, otherwise -->
   <xsl:template match="cmml:piecewise|cmml:piece|cmml:otherwise">
     <OMA>
       <OMS cd="piece1" name="{name()}"/>
       <xsl:apply-templates/>
     </OMA>
   </xsl:template>







  <!-- **************************************************** -->
  <!-- ******** Arithmetic, Algebra, and Logic ************ -->  
  <!-- **************************************************** -->

  <!-- quotient, (exp), factorial, divide, max, min, minus, plus,
       power, rem, times, root, gcd, and, or, xor, not, implies,
       forall, exists, abs, conjugate, arg, real, imaginary,
       lcm, floor, ceiling -->

   <!-- quotient -->
   <xsl:template match="cmml:quotient">
     <OMS cd="integer1" name="quotient"/>
   </xsl:template>

   <!-- exp -->
   <xsl:template match="cmml:exp">
     <OMS cd="transc1" name="exp"/>
   </xsl:template>

   <!-- factorial -->
   <xsl:template match="cmml:factorial">
     <OMS cd="integer1" name="factorial"/>
   </xsl:template>

   <!-- divide, power, times, abs -->
   <xsl:template match="cmml:divide|cmml:power|cmml:times|cmml:abs">
     <OMS cd="arith1" name="{name()}"/>
   </xsl:template>
 
   <!-- max, min -->
   <xsl:template match="cmml:apply[cmml:max]|cmml:apply[cmml:min]">
     <OMA>
       <OMS cd="minmax1" name="{name(*[1])}"/>
       <xsl:choose>
         <xsl:when test="cmml:condition">
           <OMA>
             <OMS cd="set1" name="suchthat"/>
             <xsl:apply-templates select="cmml:condition" mode="getSetname"/>
             <xsl:choose>
               <xsl:when test="*[position()=last()]=cmml:condition 
                               or *[position()=last()]=cmml:ci">
                 <xsl:apply-templates select="cmml:condition/*"/>
               </xsl:when>
               <xsl:otherwise>
                 <OMA>
                   <OMS cd="logic1" name="and"/>
                   <xsl:apply-templates select="cmml:condition/*"/>
                   <xsl:apply-templates select="*[position()=last()]"/>
                 </OMA>
               </xsl:otherwise>
             </xsl:choose>
           </OMA>
         </xsl:when>
         <xsl:otherwise>
           <OMA>
             <OMS cd="set1" name="set"/>
             <xsl:apply-templates/>
           </OMA>
         </xsl:otherwise>
       </xsl:choose>
     </OMA>
   </xsl:template>

   <!-- plus -->
   <xsl:template match="cmml:apply[cmml:plus]">
     <xsl:choose>
       <xsl:when test="descendant::cmml:set[@type='multiset'] 
                       or descendant::cmml:ci[@type='multiset']">
         <OMA>
           <OMS cd="multiset1" name="union"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </OMA>
       </xsl:when>
       <xsl:when test="descendant::cmml:set or descendant::cmml:ci[@type='set']">
         <OMA>       
           <OMS cd="set1" name="union"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </OMA>
       </xsl:when>
       <xsl:when test="count(child::*)=1">
         <OMS cd="alg1" name="zero"/>
       </xsl:when>
       <xsl:otherwise>
         <OMA>
           <OMS cd="arith1" name="plus"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </OMA>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <!-- minus -->
   <xsl:template match="cmml:apply[cmml:minus]">
     <OMA>
       <xsl:choose>
         <xsl:when test="descendant::cmml:set[@type='multiset'] 
                         or descendant::cmml:ci[@type='multiset']">
           <OMS cd="multiset1" name="setdiff"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </xsl:when>
         <xsl:when test="descendant::cmml:set or descendant::cmml:ci[@type='set']">
           <OMS cd="set1" name="setdiff"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </xsl:when>
         <xsl:when test="count(child::*)=2">
           <OMS cd="arith1" name="unary_minus"/>
           <xsl:apply-templates select="*[2]"/>
         </xsl:when>
         <xsl:otherwise>
           <OMS cd="arith1" name="minus"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </xsl:otherwise>
       </xsl:choose>
     </OMA>
   </xsl:template>

   <!-- gcd -->
   <xsl:template match="cmml:gcd">
     <OMS cd="arith1" name="gcd"/>
   </xsl:template>

   <!-- lcm -->
   <xsl:template match="cmml:apply[cmml:lcm]">
     <OMS cd="arith1" name="lcm"/>
   </xsl:template>

   <!-- root -->
   <xsl:template match="cmml:apply[cmml:root]">
     <OMA>
       <OMS cd="arith1" name="root"/>
       <xsl:choose>
	 <xsl:when test="cmml:degree">
	   <xsl:apply-templates select="*[3]"/>
           <xsl:apply-templates select="cmml:degree/*"/>
	 </xsl:when>
	 <xsl:otherwise>
           <xsl:apply-templates select="*[2]"/>
           <OMI>2</OMI>
	 </xsl:otherwise>
       </xsl:choose>
     </OMA>
   </xsl:template>

   <!-- rem -->
   <xsl:template match="cmml:rem">
     <OMS cd="integer" name="remainder"/>
   </xsl:template>

   <!-- and, or, xor, not, implies -->
   <xsl:template match="cmml:and|cmml:or|cmml:xor|cmml:not|cmml:implies">
     <OMS cd="logic1" name="{name()}"/>
   </xsl:template>

   <!-- forall, exists -->
   <xsl:template match="cmml:apply[cmml:forall|cmml:exists]">
     <OMBIND>
       <OMS cd="quant1" name="{name(*[1])}"/>
       <OMBVAR>
         <xsl:apply-templates select="cmml:bvar[position()>0]/*"/>
       </OMBVAR>
       <xsl:choose>
         <xsl:when test="cmml:condition">
           <OMA>
             <OMS cd="logic1" name="implies"/>
             <xsl:apply-templates select="cmml:condition/*"/>
             <xsl:apply-templates select="*[last()]"/>
           </OMA>
         </xsl:when>
         <xsl:otherwise>
           <xsl:apply-templates select="*[last()]"/>
         </xsl:otherwise>
       </xsl:choose>
     </OMBIND>
   </xsl:template>

   <!-- conjugate, real, imaginary -->
   <xsl:template match="cmml:conjugate|cmml:real|cmml:imaginary">
     <OMS cd="complex1" name="{name()}"/>
   </xsl:template>

   <!-- arg -->
   <xsl:template match="cmml:arg">
     <OMS cd="complex1" name="argument"/>
   </xsl:template>

   <!-- floor, ceiling -->
   <xsl:template match="cmml:floor|cmml:ceiling">
     <OMS cd="rounding1" name="{name()}"/>
   </xsl:template>








   <!-- **************************************************** -->
   <!-- ********************* Relations ******************** -->  
   <!-- **************************************************** -->

   <!-- eq, neq, gt, lt, geq, leq, equivalent, approx, factorof -->

   <!-- eq, neq, gt, lt, geq, leq -->
   <!-- Note: OpenMath's functions are binary! -->
   <xsl:template match="cmml:apply[cmml:eq|cmml:neq|cmml:gt|cmml:lt|cmml:geq|cmml:leq]">
     <OMA>
       <xsl:variable name="OP" select="*[1]"/>
       <xsl:choose>
         <xsl:when test="*[4]">
           <OMS cd="logic1" name="and"/>
           <xsl:for-each select="*[position()>2]">
             <OMA>
               <OMS cd="relation1" name="{name($OP)}"/>
               <xsl:apply-templates select="preceding-sibling::*[1]"/>
               <xsl:apply-templates select="."/>
             </OMA>
           </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
           <OMS cd="relation1" name="{name($OP)}"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </xsl:otherwise>
       </xsl:choose>
     </OMA>
   </xsl:template>

   <!-- equivalent -->
   <!-- Note: OpenMath's equivalent is an binary function! -->
   <xsl:template match="cmml:apply[cmml:equivalent]">
     <OMA>
       <xsl:choose>
         <xsl:when test="*[4]">
           <OMS cd="logic1" name="and"/>
           <xsl:for-each select="*[position()>2]">
             <OMA>
               <OMS cd="logic1" name="equivalent"/>
               <xsl:apply-templates select="preceding-sibling::*[1]"/>
               <xsl:apply-templates select="."/>
             </OMA>
           </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
           <OMS cd="logic1" name="equivalent"/>
           <xsl:apply-templates select="*[position()>1]"/>
         </xsl:otherwise>
       </xsl:choose>
     </OMA>
   </xsl:template>

   <!-- approx -->
   <xsl:template match="cmml:approx">
     <OMS cd="relation1" name="approx"/>
   </xsl:template>

   <!-- factorof -->
   <xsl:template match="cmml:factorof">
     <OMS cd="integer1" name="factorof"/>
   </xsl:template>







   <!-- **************************************************** -->
   <!-- ********** Calculus and Vector Calculus ************ -->  
   <!-- **************************************************** -->

   <!-- int, diff, partialdiff, lowlimit, uplimit, (bvar),
        (degree), divergence, grad, curl, laplacian -->

   <!-- int -->
   <xsl:template match="cmml:apply[cmml:int]">
     <OMA>
       <OMS cd="calculus1">
         <xsl:attribute name="name">
           <xsl:choose>
             <xsl:when test="cmml:lowlimit|cmml:condition|cmml:interval|cmml:domainofapplication">defint</xsl:when>
             <xsl:otherwise>int</xsl:otherwise>
           </xsl:choose>
         </xsl:attribute>
       </OMS>
       <xsl:choose>
         <xsl:when test="cmml:lowlimit and cmml:uplimit">
           <OMA>
             <OMS cd="interval1" name="interval"/>
             <xsl:apply-templates select="cmml:lowlimit/*"/>
             <xsl:apply-templates select="cmml:uplimit/*"/>
           </OMA>
         </xsl:when>
         <xsl:when test="cmml:interval">
           <xsl:apply-templates select="cmml:interval"/>
         </xsl:when>
         <xsl:when test="cmml:condition">
           <xsl:choose>
             <xsl:when test="cmml:condition/cmml:apply/cmml:in">
               <OMA>
                 <OMS cd="fns1" name="domainofapplication"/>
                 <xsl:apply-templates select="cmml:condition/cmml:apply[cmml:in]/*[3]"/>
               </OMA>
             </xsl:when>
             <xsl:otherwise>
               <xsl:comment>ERROR: Specification of domain is not supported</xsl:comment>
             </xsl:otherwise>
           </xsl:choose>
         </xsl:when>
         <xsl:when test="cmml:domainofapplication">
           <xsl:apply-templates select="cmml:domainofapplication"/>
         </xsl:when>
       </xsl:choose>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>

   <!-- diff -->
   <xsl:template match="cmml:apply[cmml:diff]">
     <OMA>
       <OMS cd="calculus1" name="diff"/>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>

   <!-- diff (with degree of differentiation -->
   <xsl:template match="cmml:apply[cmml:diff and cmml:bvar[cmml:degree]]">
     <OMA>
       <OMS cd="calculus1" name="nthdiff"/>
       <xsl:apply-templates select="cmml:bvar/cmml:degree/*"/>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>

   <!-- partialdiff -->
   <!-- (Note: There is no way to indicate degree of differentiation in OM) -->
   <xsl:template match="cmml:apply[cmml:partialdiff]">
     <OMA>
       <OMS cd="calculus1" name="partialdiff"/>
       <xsl:choose>
         <xsl:when test="*[2]=cmml:list">
           <xsl:apply-templates select="*[2]"/>
         </xsl:when>
         <xsl:when test="cmml:bvar">
           <OMA>
             <OMS cd="list1" name="list"/>
             <xsl:for-each select="cmml:bvar">
               <xsl:if test="normalize-space(cmml:ci/text())='x'">
                 <OMI>1</OMI>
               </xsl:if>
               <xsl:if test="normalize-space(cmml:ci/text())='y'">
                 <OMI>2</OMI>
               </xsl:if>
               <xsl:if test="normalize-space(cmml:ci/text())='z'">
                 <OMI>3</OMI>
               </xsl:if>
             </xsl:for-each>
           </OMA>
         </xsl:when>
       </xsl:choose>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>

   <!-- divergence, grad, curl -->
   <xsl:template match="cmml:apply[cmml:divergence|cmml:grad|cmml:curl]">
     <OMA>
       <OMS cd="veccalc1" name="{name(child::*[1])}"/>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>  

   <!-- laplacian -->
   <xsl:template match="cmml:apply[cmml:laplacian]">
     <OMA>
       <OMS cd="veccalc1" name="Laplacian"/>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>









   <!-- **************************************************** -->
   <!-- **************** Theory of sets ******************** -->  
   <!-- **************************************************** -->

   <!-- set, list, union, intersect, in, notin, subset, prsubset
        notsubset, notprsubset, seetdiff, card, cartesianproduct -->

   <!-- set -->
   <xsl:template match="cmml:set">
     <xsl:choose>
       <xsl:when test="cmml:condition">
         <OMA>
           <OMS cd="set1" name="suchthat"/>
           <xsl:apply-templates select="cmml:condition" mode="getSetname"/>
           <xsl:choose>
             <xsl:when test="*[position()=last()]=cmml:condition 
                             or *[position()=last()]=cmml:ci">
               <xsl:apply-templates select="cmml:condition/*"/>
             </xsl:when>
             <xsl:otherwise>
               <OMA>
                 <OMS cd="logic1" name="and"/>
                 <xsl:apply-templates select="cmml:condition/*"/>
                 <xsl:apply-templates select="*[position()=last()]"/>
               </OMA>
             </xsl:otherwise>
           </xsl:choose>
         </OMA>
       </xsl:when>
       <xsl:otherwise>
         <OMA>
           <OMS cd="set1" name="set"/>
           <xsl:apply-templates/>
         </OMA>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <!-- set (type multiset) -->
   <xsl:template match="cmml:set[@type='multiset']">
     <xsl:choose>
       <xsl:when test="cmml:condition">
         <OMA>
           <OMS cd="set1" name="suchthat"/>
           <xsl:apply-templates select="cmml:condition" mode="getSetname"/>
           <xsl:choose>
             <xsl:when test="*[position()=last()]=cmml:condition or *[position()=last()]=cmml:ci">
               <xsl:apply-templates select="cmml:condition/*"/>
             </xsl:when>
             <xsl:otherwise>
               <OMA>
                 <OMS cd="logic1" name="and"/>
                 <xsl:apply-templates select="cmml:condition/*"/>
                 <xsl:apply-templates select="*[position()=last()]"/>
               </OMA>
             </xsl:otherwise>
           </xsl:choose>
         </OMA>
       </xsl:when>
       <xsl:otherwise>
         <OMA>
           <OMS cd="multiset1" name="multiset"/>
           <xsl:apply-templates/>
         </OMA>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <!-- list -->
   <xsl:template match="cmml:list">
     <xsl:choose>
       <xsl:when test="cmml:condition">
         <OMA>
           <OMS cd="list1" name="suchthat"/>
           <xsl:apply-templates select="cmml:condition" mode="getSetname"/>
           <xsl:choose>
             <xsl:when test="*[position()=last()]=cmml:condition or *[position()=last()]=cmml:ci">
               <xsl:apply-templates select="cmml:condition/*"/>
             </xsl:when>
             <xsl:otherwise>
               <OMA>
                 <OMS cd="logic1" name="and"/>
                 <xsl:apply-templates select="cmml:condition/*"/>
                 <xsl:apply-templates select="*[position()=last()]"/>
               </OMA>
             </xsl:otherwise>
           </xsl:choose>
         </OMA>
       </xsl:when>
       <xsl:otherwise>
         <OMA>
           <OMS cd="list1" name="list"/>
           <xsl:apply-templates/>
         </OMA>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <!-- union, intersect, in, notin, subset, notsubset, notprsubset, 
        setdiff, card, cartesianproduct -->
   <xsl:template match="cmml:apply[cmml:union|cmml:intersect|cmml:in|cmml:notin
                        |cmml:subset|cmml:notsubset|cmml:notprsubset|cmml:setdiff
                        |cmml:card|cmml:cartesianproduct]">
     <OMA>
       <OMS>
         <xsl:attribute name="cd">
           <xsl:choose>
             <xsl:when test="descendant::*[@type='multiset']">multiset1</xsl:when>
             <xsl:otherwise>set1</xsl:otherwise>
           </xsl:choose>
         </xsl:attribute>
         <xsl:attribute name="name">
           <xsl:choose>
             <xsl:when test="*[1]=cmml:card">size</xsl:when>
             <xsl:when test="*[1]=cmml:cartesianproduct">cartesian_product</xsl:when>
             <xsl:otherwise><xsl:value-of select="name(*[1])"/></xsl:otherwise>
           </xsl:choose>
         </xsl:attribute>
       </OMS>
       <xsl:apply-templates select="*[position()>1]"/>
     </OMA>
   </xsl:template>









   <!-- **************************************************** -->
   <!-- ************** Sequences and Series **************** -->  
   <!-- **************************************************** -->

   <!-- sum, product, limit, (tendsto) -->

   <!-- sum, product -->
   <xsl:template match="cmml:apply[cmml:sum|cmml:product]">
     <OMA>
       <OMS cd="arith1" name="{name(*[1])}"/>
       <xsl:choose>
         <xsl:when test="cmml:lowlimit and cmml:uplimit">
           <OMA>
             <OMS cd="interval1" name="interval"/>
             <xsl:apply-templates select="cmml:lowlimit/*"/>
             <xsl:apply-templates select="cmml:uplimit/*"/>
           </OMA>
         </xsl:when>
         <xsl:when test="cmml:interval">
           <xsl:apply-templates select="cmml:interval"/>
         </xsl:when>
         <xsl:when test="cmml:condition">
           <xsl:choose>
             <xsl:when test="cmml:condition/cmml:apply/cmml:in">
               <OMA>
                 <OMS cd="fns1" name="domainofapplication"/>
                 <xsl:apply-templates select="cmml:condition/cmml:apply[cmml:in]/*[3]"/>
               </OMA>
             </xsl:when>
             <xsl:otherwise>
               <xsl:comment>ERROR: Specification of domain is not supported</xsl:comment>
             </xsl:otherwise>
           </xsl:choose>
         </xsl:when>
         <xsl:when test="cmml:domainofapplication">
           <xsl:apply-templates select="cmml:domainofapplication"/>
         </xsl:when>
       </xsl:choose>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>

   <!-- limit -->
   <xsl:template match="cmml:apply[cmml:limit]">
     <OMA>
       <OMS cd="limit1" name="limit"/>
       <xsl:choose>
         <xsl:when test="cmml:condition">
           <xsl:variable name="type" 
             select="cmml:condition/cmml:apply/cmml:tendsto/@type"/>
           <xsl:apply-templates 
             select="cmml:condition/cmml:apply[cmml:tendsto]/*[last()]"/>
           <OMS cd="limit1">
             <xsl:attribute name="name">
               <xsl:choose>
                 <xsl:when test="$type='above'">above</xsl:when>
                 <xsl:when test="$type='below'">below</xsl:when>
                 <xsl:when test="$type='all'">both_sides</xsl:when> 
                 <xsl:otherwise>null</xsl:otherwise> <!-- default -->
               </xsl:choose>
             </xsl:attribute>
           </OMS>
         </xsl:when>
         <xsl:when test="cmml:lowlimit">
           <xsl:apply-templates select="cmml:lowlimit/*"/>
           <OMS cd="limit1" name="null"/>
         </xsl:when>
         <xsl:when test="cmml:uplimit">
           <xsl:apply-templates select="cmml:uplimit/*"/>
           <OMS cd="limit1" name="null"/>
         </xsl:when>
       </xsl:choose>
       <xsl:apply-templates select="*[last()]"/>
     </OMA>
   </xsl:template>

   <!-- tendsto -->
   <!-- (no equivalent in OpenMath) -->
   <xsl:template match="cmml:apply[cmml:tendsto]">
     <xsl:comment>ERROR: "Tendsto" is not supported in OpenMath</xsl:comment>
   </xsl:template>




   <!-- **************************************************** -->
   <!-- ********** Elementary Classical Functions ********** -->  
   <!-- **************************************************** -->

   <!-- All trig functions, exp, ln, log -->

   <!-- exp (in Arithmatic, Algebra and Logic section) -->

   <!-- ln -->
   <xsl:template match="cmml:ln">
     <OMS cd="transc1" name="ln"/>
   </xsl:template>

   <!-- log -->
   <xsl:template match="cmml:apply[cmml:log]">
     <OMA>
       <OMS cd="transc1" name="log"/>
       <xsl:choose>
         <xsl:when test="cmml:logbase">
           <xsl:apply-templates select="cmml:logbase/*"/>
           <xsl:apply-templates select="*[3]"/>
         </xsl:when>
         <xsl:otherwise>
           <OMI>10</OMI>
           <xsl:apply-templates select="*[2]"/>
         </xsl:otherwise>
       </xsl:choose>
     </OMA>
   </xsl:template>

   <!-- (all trig functions) -->
   <xsl:template match="cmml:sin|cmml:cos|cmml:tan|cmml:sec|cmml:sec
                        |cmml:csc|cmml:cot|cmml:sinh|cmml:cosh|cmml:tanh
                        |cmml:sech|cmml:sech|cmml:csch|cmml:coth|cmml:arcsin
                        |cmml:arccos|cmml:arctan|cmml:arcsec|cmml:arcsec
                        |cmml:arccsc|cmml:arccot|cmml:arcsinh|cmml:arccosh
                        |cmml:arctanh|cmml:arcsech|cmml:arcsech|cmml:arccsch
                        |cmml:arccoth">
     <OMS cd="transc1" name="{name()}"/>
   </xsl:template>






   <!-- **************************************************** -->
   <!-- ******************* Statistics ********************* -->  
   <!-- **************************************************** -->

   <!-- mean, sdev, variance, median, mode, moment, (momentabout) -->

   <!-- mean, sdev, variance -->
   <xsl:template match="cmml:apply[cmml:mean|cmml:sdev|cmml:variance]">
     <OMA>
       <OMS>
	 <xsl:attribute name="cd">
	   <xsl:choose>
	     <xsl:when test="*[3]">s_data1</xsl:when>
	     <xsl:otherwise>s_dist1</xsl:otherwise>
	   </xsl:choose>
	 </xsl:attribute>
	 <xsl:attribute name="name">
           <xsl:value-of select="name(*[1])"/>
	 </xsl:attribute>
       </OMS>
       <xsl:apply-templates select="*[position()>1]"/>
     </OMA>
   </xsl:template>

   <!-- mode -->
   <xsl:template match="cmml:mode">
     <OMS cd="s_data1" name="mode"/>
   </xsl:template>

   <!-- median -->
   <xsl:template match="cmml:median">
     <OMS cd="s_data1" name="median"/>
   </xsl:template>

   <!-- moment, momentabout -->
   <xsl:template match="cmml:apply[cmml:moment]">
     <OMA>
       <xsl:choose>
         <xsl:when test="*[5]">
           <OMS cd="s_data1" name="moment"/>
         </xsl:when>
         <xsl:otherwise>
           <OMS cd="s_dist1" name="moment"/>
         </xsl:otherwise>
       </xsl:choose>
       <xsl:apply-templates select="cmml:degree/*"/>
       <xsl:apply-templates select="cmml:momentabout/*"/>
       <xsl:apply-templates select="*[position()>3]"/>
     </OMA>
   </xsl:template>





   <!-- **************************************************** -->
   <!-- ******************** Linear Algebra **************** -->  
   <!-- **************************************************** -->

   <!-- vector, matrix, matrixrow, determinant, transpose, 
        selector, vectorproduct, scalarproduct, outerproduct -->

   <!-- vector -->
   <xsl:template match="cmml:vector">
     <OMA>
       <OMS cd="linalg3" name="vector"/>
       <xsl:apply-templates/>
     </OMA>
   </xsl:template>

   <!-- matrix, matrixrow -->
   <xsl:template match="cmml:matrix|cmml:matrixrow">
     <OMA>
       <OMS cd="linalg2" name="{name()}"/>
       <xsl:apply-templates/>
     </OMA>
   </xsl:template>


   <!-- determinant, transpose -->
   <xsl:template match="cmml:determinant|cmml:transpose">
     <OMS cd="linalg1" name="{name()}"/>
   </xsl:template>

   <!-- selector -->
   <xsl:template match="cmml:apply[cmml:selector]">
     <xsl:choose>
       <xsl:when test="count(child::*) &lt; 3">
         <xsl:comment>ERROR: No arguement given</xsl:comment>
       </xsl:when>
       <xsl:when test="*[2]=cmml:list or *[2]=cmml:ci[@type='list']">
         <xsl:comment>ERROR: OpenMath does not support list selector</xsl:comment>
       </xsl:when>
       <xsl:when test="(*[2]=cmml:matrix or *[2]=cmml:ci[@type='matrix']) and count(child::*)!=4">
         <xsl:comment>ERROR: OpenMath's matrix selector cannot select a row of a matrix</xsl:comment>
       </xsl:when>
       <xsl:when test="(*[2]=cmml:vector or *[2]=cmml:matrixrow 
                       or *[2]=cmml:ci[@type='vector' or @type='matrixrow']) and count(child::*)=4">
         <xsl:comment>ERROR: Too many arguments for vector or matrixrow</xsl:comment>
       </xsl:when>
       <xsl:otherwise>
         <OMA>
           <OMS cd="linalg1">
             <xsl:attribute name="name">
               <xsl:choose>
                 <xsl:when test="count(child::*)=4">matrix_selector</xsl:when> <!-- matrix -->
                 <xsl:when test="count(child::*)=3">vector_selector</xsl:when> <!-- vector or matrixrow -->
                 <xsl:otherwise>vector_selector</xsl:otherwise>
               </xsl:choose>
             </xsl:attribute>
           </OMS>
           <xsl:choose>
             <xsl:when test="count(child::*)=3"> <!-- vector or matrixrow -->
               <xsl:apply-templates select="*[3]"/>
               <xsl:apply-templates select="*[2]"/>
             </xsl:when>
             <xsl:when test="count(child::*)=4"> <!-- matrix -->
               <xsl:apply-templates select="*[4]"/>
               <xsl:apply-templates select="*[3]"/>
               <xsl:apply-templates select="*[2]"/>
             </xsl:when>
           </xsl:choose>
         </OMA>
       </xsl:otherwise>
     </xsl:choose>
    </xsl:template>
          
   <!-- vectorproduct -->
   <xsl:template match="cmml:vectorproduct">
     <OMS cd="linalg1" name="vector_product"/>
   </xsl:template>

   <!-- scalarproduct -->
   <xsl:template match="cmml:scalarproduct">
     <OMS cd="linalg1" name="scalar_product"/>
   </xsl:template>

   <!-- outerproduct -->
   <xsl:template match="cmml:outerproduct">
     <OMS cd="linalg1" name="outer_product"/>
   </xsl:template>






   <!-- **************************************************** -->
   <!-- ************ Semantic mapping elements ************* -->  
   <!-- **************************************************** -->

   <!-- semantics, annotation, annotation-xml -->

   <!-- semantics -->
   <xsl:template match="cmml:semantics">
     <OMATTR>
       <OMATP>
         <xsl:apply-templates select="cmml:annotation"/>
         <xsl:apply-templates select="cmml:annotation-xml"/>
       </OMATP>
       <xsl:apply-templates select="*[1]"/>
     </OMATTR>
   </xsl:template>

   <!-- annotation, annotation-xml -->
   <!-- (OpenMath does not support other encodings such as Maple and Mathematica) -->
   <xsl:template match="cmml:annotation|cmml:annotation-xml">
     <xsl:choose>
       <xsl:when test="contains(@encoding,'MathML')">
         <OMS cd="altenc" name="MathML_encoding"/>
         <OMXML><xsl:value-of select="normalize-space(.)"/></OMXML>
       </xsl:when>
       <xsl:when test="contains(@encoding,'LaTeX')">
         <OMS cd="altenc" name="LaTeX_encoding"/>
         <OMSTR><xsl:value-of select="normalize-space(.)"/></OMSTR>
       </xsl:when>
       <xsl:otherwise>
         <xsl:comment>OpenMath does not support other encodings</xsl:comment>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>






   <!-- **************************************************** -->
   <!-- ************ Constants and symbol elements ********* -->  
   <!-- **************************************************** -->
 
   <!-- integers, reals, rationals, naturalnumbers, complexes, primes
        exponentiale, imaginaryi, notanumber, true, fasle, emptyset,
        pi, eulergamma, infinity -->

   <!-- integers -->
   <xsl:template match="cmml:integers">
     <OMS cd="setname1" name="Z"/>
   </xsl:template>

   <!-- reals -->
   <xsl:template match="cmml:reals">
     <OMS cd="setname1" name="R"/>
   </xsl:template>

   <!-- rationals -->
   <xsl:template match="cmml:rationals">
     <OMS cd="setname1" name="Q"/>
   </xsl:template>

   <!-- naturalnumbers -->
   <xsl:template match="cmml:naturalnumbers">
     <OMS cd="setname1" name="N"/>
   </xsl:template>

   <!-- complexes -->
   <xsl:template match="cmml:complexes">
     <OMS cd="setname1" name="C"/>
   </xsl:template>

   <!-- primes -->
   <xsl:template match="cmml:primes">
     <OMS cd="setname1" name="P"/>
   </xsl:template>

   <!-- exponentiale -->
   <xsl:template match="cmml:exponentiale">
     <OMS cd="nums1" name="e"/>
   </xsl:template>

   <!-- imaginaryi -->
   <xsl:template match="cmml:imaginaryi">
     <OMS cd="nums1" name="i"/>
   </xsl:template>

   <!-- notanumber -->
   <xsl:template match="cmml:notanumber">
     <OMS cd="nums1" name="NaN"/>
   </xsl:template>

   <!-- true -->
   <xsl:template match="cmml:true">
     <OMS cd="logic1" name="true"/>
   </xsl:template>

   <!-- false -->
   <xsl:template match="cmml:false">
     <OMS cd="logic1" name="false"/>
   </xsl:template>

   <!-- emptyset -->
   <xsl:template match="cmml:emptyset">
     <xsl:choose>
       <xsl:when test="parent::*/descendant::*[@type='multiset']">
         <OMS cd="multiset1" name="emptyset"/>
       </xsl:when>
       <xsl:otherwise>
         <OMS cd="set1" name="emptyset"/>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <!-- pi -->
   <xsl:template match="cmml:pi">
     <OMS cd="nums1" name="pi"/>
   </xsl:template>
   
   <!-- eulergamma -->
   <xsl:template match="cmml:eulergamma">
     <OMS cd="nums1" name="gamma"/>
   </xsl:template>

   <!-- infinity -->
   <xsl:template match="cmml:infinity">
     <OMS cd="nums1" name="infinity"/>
   </xsl:template>





 
   <!-- **************************************************** -->
   <!-- ***************** Helper functions ***************** -->  
   <!-- **************************************************** -->

  <!-- This template is for converting from text children in rational or complex numbers to actual OMIs, OMFs, or OMSs -->
  <xsl:template match="text()" mode="convert">
    <xsl:variable name="NUM" select="normalize-space(.)"/>
    <xsl:choose>
      <xsl:when test="$NUM='1'">
        <OMS cd="alg1" name="one"/>
      </xsl:when>
      <xsl:when test="$NUM='0'">
        <OMS cd="alg1" name="zero"/>
      </xsl:when>
      <xsl:when test="NUM='&pi;'">
        <OMS cd="nums1" name="pi"/>
      </xsl:when>
      <xsl:when test="$NUM='&ee;'">
        <OMS cd="nums1" name="e"/>
      </xsl:when>
      <xsl:when test="$NUM='&ExponentialE;'">
        <OMS cd="nums1" name="e"/>
      </xsl:when>
      <xsl:when test="$NUM='&gamma;'">
        <OMS cd="nums1" name="gamma"/>
      </xsl:when>
      <xsl:when test="$NUM='&infin;'">
        <OMS cd="nums1" name="infinity"/>
      </xsl:when>
      <xsl:when test="$NUM='&infty;'">
        <OMS cd="nums1" name="infinity"/>
      </xsl:when>
      <xsl:when test="$NUM='&true;'">
        <OMS cd="logic1" name="true"/>
      </xsl:when>
      <xsl:when test="$NUM='&false;'">
        <OMS cd="logic1" name="false"/>
      </xsl:when>
      <xsl:when test="$NUM='&NotANumber;'">
        <OMS cd="nums1" name="NaN"/>
      </xsl:when>
      <xsl:when test="$NUM='&NaN;'">
        <OMS cd="nums1" name="NaN"/>
      </xsl:when>
      <xsl:when test="$NUM='&ii;'">
        <OMS cd="nums1" name="i"/>
      </xsl:when>
      <xsl:when test="$NUM='&ImaginaryI;'">
        <OMS cd="nums1" name="i"/>
      </xsl:when>
      <xsl:when test="contains($NUM,'.')">
        <OMF dec="{$NUM}"/>
      </xsl:when>
      <xsl:otherwise>
        <OMI><xsl:value-of select="$NUM"/></OMI>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>


   <xsl:template match="cmml:condition" mode="getLimit">
     <xsl:variable name="type" select="cmml:tendsto/@type"/>
     <xsl:apply-templates select="cmml:apply[cmml:tendsto]/*[last()]"/>
     <OMS cd="limit1">
       <xsl:attribute name="name">
         <xsl:choose>
           <xsl:when test="$type='above'">above</xsl:when>
           <xsl:when test="$type='below'">below</xsl:when>
           <xsl:when test="$type='all'">both_sides</xsl:when> 
           <xsl:otherwise>null</xsl:otherwise> <!-- default -->
         </xsl:choose>
       </xsl:attribute>
     </OMS>
   </xsl:template>


   <xsl:template match="cmml:condition" mode="getSetname">
     <xsl:choose>
       <xsl:when test="descendant::cmml:apply/cmml:in">
         <xsl:apply-templates select="descendant::cmml:apply[cmml:in][position()=1]/*[3]"/>
       </xsl:when>
       <xsl:when test="normalize-space(descendant::cmml:ci/text())=i or
                       normalize-space(descendant::cmml:ci/text())=j or
                       normalize-space(descendant::cmml:ci/text())=k or
                       normalize-space(descendant::cmml:ci/text())=l or
                       normalize-space(descendant::cmml:ci/text())=m or
                       normalize-space(descendant::cmml:ci/text())=n or
                       normalize-space(descendant::cmml:ci/text())=I or
                       normalize-space(descendant::cmml:ci/text())=J or
                       normalize-space(descendant::cmml:ci/text())=K or
                       normalize-space(descendant::cmml:ci/text())=L or
                       normalize-space(descendant::cmml:ci/text())=M or
                       normalize-space(descendant::cmml:ci/text())=N">
         <OMS cd="setname1" name="N"/>
       </xsl:when>
       <xsl:when test="normalize-space(descendant::cmml:ci/text())=z or
                       normalize-space(descendant::cmml:ci/text())=w">
         <OMS cd="setname1" name="C"/>
       </xsl:when>
       <xsl:otherwise>
         <OMS cd="setname1" name="R"/>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

</xsl:stylesheet>



