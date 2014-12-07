<?xml version="1.0" encoding="UTF-8"?>
<!-- An XSL style sheet for producing RDF from OMDoc (Open Mathematical Documents). 
     $Id: omdoc-ST-model.mod 8014 2008-09-07 19:52:19Z kohlhase $
     $HeadURL: https://svn.omdoc.org/repos/omdoc/branches/omdoc-1.2/dtd/omdoc-ST-model.mod $

     Copyright (c) 2006 Michael Kohlhase, Octavian Druta, Cristoph Lange

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2.1 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   -->
<!DOCTYPE rdf:RDF [
	<!ENTITY oso "http://www.mathweb.org/omdoc/system-ontology#">
]>   
   
<xsl:stylesheet version="1.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:omdoc="http://www.mathweb.org/omdoc" 
                xmlns:oso="http://www.mathweb.org/omdoc/system-ontology#" 
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:func="http://exslt.org/functions" 
                extension-element-prefixes="func"
                exclude-result-prefixes="omdoc oso dc">
                
  
                
  <xsl:output method="xml" indent="yes"/>
  
  <!-- the filename parameters comes from the command line -->
  <xsl:param name="filename"/>
  
  <!-- match the omdoc element and apply templates for anything inside its enclosures -->
  <xsl:template match="omdoc:omdoc">
    <rdf:RDF>
      <xsl:apply-templates/>
    </rdf:RDF>
  </xsl:template>
  
  <!-- matches any theory -->
  <xsl:template match="omdoc:theory">
    <xsl:message>processing theory: <xsl:value-of select="@xml:id"/>
    </xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <xsl:apply-templates select="omdoc:imports"/>
      <rdf:type rdf:resource="&oso;Theory"/>
      <!-- if the theory has a parent then specify it -->
      <xsl:if test="parent::omdoc:theory/@xml:id != ''">
        <oso:homeTheory rdf:resource="{concat($filename,'#',parent::omdoc:theory/@xml:id)}"/>
      </xsl:if>
    </rdf:Description>    
    <!-- apply templates for anything that's inside a theory, except nested theories and imports-->
    <xsl:apply-templates select="*[not(self::omdoc:theory or self::omdoc:imports)]"/>
    <!-- process nested theories -->
    <xsl:apply-templates select="omdoc:theory">
    </xsl:apply-templates>
  </xsl:template>
  
  <!-- matches metadata content -->
  <xsl:template name="process-metadata">
    <xsl:copy-of select="omdoc:metadata/*"/>
    <!-- in case the element's metadata doesn't contain a dc:creator element then
         retrieve the dc:creator of the OMDoc document -->
    <xsl:if test="not(omdoc:metadata/dc:creator)">
      <xsl:copy-of select="/omdoc:omdoc/omdoc:metadata/dc:creator"/>
    </xsl:if>
  </xsl:template>
  
  <!-- this function places the proper reference for a resource -->
  <func:function name="omdoc:process-reference">
    <xsl:param name="for"/>
    <xsl:param name="id"/>
    <xsl:param name="file"/>
    <xsl:message>  processing reference</xsl:message>
    <xsl:choose>
      <!-- the case in which there is no reference -->
      <xsl:when test="not($for)"><func:result select="concat($file,'#',$id)" /></xsl:when>
      <!-- the referenced resource is located in the same file -->
      <xsl:when test="substring-before($for,'#')=''"><func:result select="concat($file,$for)" /></xsl:when>
      <!-- the referenced resource is located in a remote file -->
      <xsl:otherwise><func:result select="$for" /></xsl:otherwise>
    </xsl:choose>
  </func:function>
  
  <!-- matches any symbol -->
  <xsl:template match="omdoc:symbol">
    <xsl:message>  processing symbol: <xsl:value-of select="@xml:id"/>
    </xsl:message>
    <!--<xsl:value-of select="my:count-elements()" />-->
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <rdf:type rdf:resource="&oso;Symbol"/>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <xsl:call-template name="process-metadata"/>
    </rdf:Description>
  </xsl:template>
  
  <!-- matches any assertion of any type -->
  <xsl:template match="omdoc:assertion">
    <xsl:message>processing assertion: <xsl:value-of select="@xml:id"/></xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <xsl:choose>
        <!-- I am not sure whether this test will match the types if they're written with a different case -->
        <xsl:when test="@type='lemma' or 
                        @type='theorem' or 
                        @type='corollary' or 
                        @type='proposition' or 
                        @type='assertion' or
                        @type='falseconjecture' or
                        @type='obligation' or
                        @type='postulate' or
                        @type='formula' or
                        @type='assumption' or
                        @type='rule'">                        
          <rdf:type rdf:resource="{concat('&oso;',@type)}"/>
        </xsl:when>
        <xsl:when test="not(@type)"><rdf:type rdf:resource="&oso;Assertion"/></xsl:when>
        <xsl:otherwise>  type: <xsl:value-of select="@type"/> :not supported</xsl:otherwise>
      </xsl:choose>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <xsl:call-template name="process-metadata"/>
    </rdf:Description>
  </xsl:template>
  
  <!-- matches any axiom -->
  <xsl:template match="omdoc:axiom">
    <xsl:message>  processing axiom: <xsl:value-of select="@xml:id"/></xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <rdf:type rdf:resource="&oso;Axiom"/>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <xsl:call-template name="process-metadata"/>
    </rdf:Description>
  </xsl:template>
  
  <!-- matches any example -->
  <xsl:template match="omdoc:example">
    <xsl:message>  processing example: <xsl:value-of select="@xml:id"/>
    </xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <rdf:type rdf:resource="&oso;Example"/>
      <oso:exemplifies rdf:resource="{concat($filename,'#',@for)}"/>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <xsl:call-template name="process-metadata"/>
    </rdf:Description>
  </xsl:template>
  
  <!-- matches any import -->
  <xsl:template match="omdoc:imports">
    <xsl:message>  processing import: <xsl:value-of select="@xml:id"/>
    </xsl:message>
    <oso:imports rdf:resource="{@from}"/>
  </xsl:template>
  
  <!-- matches any proof -->
  <xsl:template match="omdoc:proof">
    <xsl:message>  processing proof: <xsl:value-of select="@xml:id"/></xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
    <rdf:type rdf:resource="&oso;Proof"/>
    <oso:proves rdf:resource="{@for}"/>
    <!-- TODO: homeTheory is going to be condition by its parent::theory -->
    <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
    <xsl:call-template name="process-metadata"/>
    </rdf:Description>
  </xsl:template>
  
  <!-- matches any definition -->
  <xsl:template match="omdoc:definition">
    <xsl:message>  processing definition: <xsl:value-of select="@xml:id"/></xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <rdf:type rdf:resource="&oso;Definition"/>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <oso:defines rdf:resource="{@for}"/>
      <xsl:call-template name="process-metadata"/>
      <!-- TODO: handle the @type attribute, see: http://bugzilla.mathweb.org:8000/show_bug.cgi?id=1258 -->
     </rdf:Description>
  </xsl:template>
  
  <!-- We need to create a property (in the ontology) to model the relation
  alternative/@for="#some-definition". check bug http://bugzilla.mathweb.org:8000/show_bug.cgi?id=1260 -->
  <!-- matches any alternative definition -->
  <xsl:template match="omdoc:alternative">
    <xsl:message>  processing alternative definition: <xsl:value-of select="@xml:id"/></xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <rdf:type rdf:resource="&oso;Alternative"/>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <oso:defines rdf:resource="{@for}"/>
      <xsl:call-template name="process-metadata"/>
     </rdf:Description>
  </xsl:template>
  
  <!-- display the elements for which nothing has been implemented yet -->
  <xsl:template match="*">
    <xsl:message>  can't deal with element <xsl:value-of select="local-name()"/> yet!</xsl:message>
  </xsl:template>
  
  <!-- Offline until we extend the system ontology to handle this kind of assumption.
  <xsl:template match="omdoc:assumption">
    <xsl:message>  processing assumption: <xsl:value-of select="@xml:id"/></xsl:message>
    <rdf:Description rdf:about="{omdoc:process-reference(@for, @xml:id, $filename)}">
      <rdf:type rdf:resource="&oso;Assumption"/>
      <oso:homeTheory rdf:resource="{omdoc:process-reference(@for, parent::omdoc:theory/@xml:id, $filename)}"/>
      <xsl:call-template name="process-metadata"/>
     </rdf:Description>
  </xsl:template>
  -->
</xsl:stylesheet>
