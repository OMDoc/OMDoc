<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version="1.0">
<!-- Licensed Materials - Property of IBM (C) Copyright IBM Corp. 2000 All Rights Reserved. -->

      <xsl:variable name="readmeURI">XML_Schema_Quality_Checker_Readme.html</xsl:variable>

	<xsl:template match="/">
		<xsl:apply-templates select="schemaAnalysis"/>
	</xsl:template>

	<xsl:template name="frontMatter">
		<xsl:if test="$simple='false'">
			<h1>IBM XML Schema Quality Checker Results</h1>
		</xsl:if>
		<i>Schema Quality Checker Version <xsl:value-of select="@SchemaQualityCheckerVersion"/> Cost <xsl:value-of select="@cost"/></i><br/>
      		<xsl:if test="@haltAtFirstError='true'">
		       <p><i>Note: Schema Quality Checker halted after the first error was encountered</i></p>
		</xsl:if>
		<xsl:if test="$simple='false'">
			<p><font size="-1">For more information, see the XML_Schema_Quality_Checker_Readme.html file.</font></p>
		</xsl:if>
	</xsl:template>
	
	
	<xsl:template match="schemaAnalysis">
		<html>
			<head>	
				<title>Schema Quality Checker version <xsl:value-of select="@SchemaQualityCheckerVersion"/></title>
				<style type="text/css">
					body { font-family: courier;
						     font-size: 8pt; }
					table { font-family: courier;
						     font-size: 8pt; }
				</style>
			</head>
			<body bgcolor="#FFFFFF">
				<xsl:call-template name="frontMatter"/>
				 <xsl:choose>
					<xsl:when test="(count(file)=1) and (count(file/noErrorFound)=0) and (count(file/error)=0)">
						<h2>No Schema files matching <xsl:value-of select="file/@filename"/> were found.</h2>
					</xsl:when>
					<xsl:otherwise>
						<xsl:if test="$simple='false'">
							<h2> Error report summary</h2>
							<table border="2">
								<tr>
									<th>file location</th>
									<th>number of errors</th>
								</tr>
								<xsl:for-each select="file">
								<!--<xsl:sort select="@filename"/>-->
									<tr>
										<td>
											<xsl:choose>
												<xsl:when  test="count(error)>0">
													<xsl:variable name="anchorName"><xsl:value-of select="@filename"></xsl:value-of></xsl:variable>
													<a href="#{$anchorName}"><xsl:value-of select="@filename"/></a>
												</xsl:when>
												<xsl:otherwise>
													<xsl:value-of select="@filename"/>
												</xsl:otherwise>
											</xsl:choose>
										</td>
										<td><center><xsl:value-of select="count(error)"/></center></td>
									</tr>
								</xsl:for-each>
							</table>	
							<br/>
						</xsl:if>
						<xsl:choose>
							<xsl:when test="count(file/error) >0 ">
								<xsl:if test="$simple='false'">
									<h2> Details </h2>
								</xsl:if>
								<xsl:apply-templates select="file"/>			
							</xsl:when>
							<xsl:otherwise>
								<i>No errors found</i>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:otherwise>
				</xsl:choose>
			</body>
		</html>
	</xsl:template>	

	<xsl:template match="file">
		<xsl:choose>
			<xsl:when  test="count(error)>0">
				<xsl:variable name="anchorName"><xsl:value-of select="@filename"></xsl:value-of></xsl:variable>
				<a name="{$anchorName}"></a>
				<table border="1">
					<tr BGCOLOR="#CCCCFF" > <!--  -->
						<th  colspan="5" ><xsl:call-template name="formatFilename"/></th>
					</tr>
					<xsl:if test="$simple='false'">
						<tr>
							<th width="10%"><center><font size="-1">line</font></center></th>
							<th width="10%"><center><font size="-1">column</font></center></th>
							<th width="10%"><center><font size="-1">severity</font></center></th>
							<th width="10%"><center><font size="-1">error type</font></center></th>
							<th width="60%"><center><font size="-1">location</font></center></th>
						</tr>
					</xsl:if>
					<xsl:if test="$simple='true'">
						<tr>
							<th width="10%"><center><font size="-1">line</font></center></th>
							<th width="10%"><center><font size="-1">column</font></center></th>
							<th width="10%"><center><font size="-1">severity</font></center></th>
							<th width="10%"><center><font size="-1">error type</font></center></th>
							<th width="60%"><center><font size="-1">location</font></center></th>
						</tr>
					</xsl:if>
					<xsl:for-each select="error">
						<xsl:sort select="concat(source/@location,source/@line,source/@column)"/>
						<tr>
							<td><center><xsl:value-of select="source/@line"/></center></td>
							<td><center><xsl:value-of select="source/@column"/></center></td>
							<xsl:choose>
								<xsl:when test="@severity='2'">
									<td><center> FATAL ERROR </center></td>
								</xsl:when>
								<xsl:otherwise>	
									<xsl:choose>
										<xsl:when test="@severity='0'">
											<td><center> WARNING </center></td>
										</xsl:when>
										<xsl:when test="@severity='1'">
										       <td><center> ERROR </center></td>
										</xsl:when>
										<xsl:otherwise>	
											<td>
												<font size="-1"> unknown severity - stylesheet needs to be updated <br>Notify <a href="mailto:xschema@us.ibm.com">IBM</a> to report this problem</br> </font>
											</td>
										</xsl:otherwise>
									</xsl:choose>
								</xsl:otherwise>
							</xsl:choose>
							<xsl:choose>
								<xsl:when test="@type='0'">
									<td><center> Well formedness </center></td>
								</xsl:when>
								<xsl:when test="@type='1'">
							       	<td><center> Invalid </center></td>
								</xsl:when>
								<xsl:when test="@type='2'">
							       	<td><center> Constraint </center></td>
								</xsl:when>
								<xsl:otherwise>	
									<td> 
										<font size="-1"> unknown type - stylesheet needs to be updated <br>Notify <a href="mailto:xschema@us.ibm.com">IBM</a> to report this problem</br></font>  
									</td>
								</xsl:otherwise>
							</xsl:choose>
							<td>
								<xsl:call-template name="formatLocation"/>
							</td>
						</tr>	
						<tr>

							<!-- convert links in message -->
							<td colspan="5">
							<xsl:for-each select="message/speclink | message/text()">
								<xsl:choose>
									<xsl:when test="name()!='speclink'">
										<xsl:value-of select="."/>
									</xsl:when>
									<xsl:otherwise>
										<a href="{@href}">
											<xsl:value-of select="text()"/>
										</a>
									</xsl:otherwise>	
								</xsl:choose>
							</xsl:for-each>

							</td>

						</tr>				
					</xsl:for-each>
				</table>
			</xsl:when>
			<!--	<xsl:otherwise>
				<table border="1">
					<tr BGCOLOR="#CCCCFF">
						<th > No error found at <xsl:text> </xsl:text> <xsl:value-of select="@filename"/> </th>
					</tr>
				</table>
			</xsl:otherwise>
			-->
		</xsl:choose>		
	</xsl:template>
	
	<xsl:template name="formatLocation">
		<xsl:variable name="location"><xsl:value-of select="source/@location"/></xsl:variable>  
		<xsl:choose>
			<xsl:when test="string-length(source/@location)=0">
				<br/>
			</xsl:when>
			<xsl:when test="string-length(source/@location)&lt;60">
				<a href="{$location}"> <xsl:value-of select="source/@location"/> </a>
			</xsl:when>
			<xsl:when test="string-length(source/@location)&gt;180">
				<a href="{$location}">
					<xsl:value-of select="substring(source/@location,1,60)"/><br/>
					<xsl:value-of select="substring(source/@location,61,60)"/><br/>
					<xsl:value-of select="substring(source/@location,121,60)"/><br/>
					<xsl:value-of select="substring(source/@location,181)"/> 
				</a>
			</xsl:when>
			<xsl:when test="string-length(source/@location)&gt;120">
				<a href="{$location}">
					<xsl:value-of select="substring(source/@location,1,60)"/><br/>
					<xsl:value-of select="substring(source/@location,61,60)"/><br/>
					<xsl:value-of select="substring(source/@location,121)"/><br/>
				</a>
			</xsl:when>
			<xsl:otherwise>
				<a href="{$location}"> 
					<xsl:value-of select="substring(source/@location,1,60)"/><br/>
					<xsl:value-of select="substring(source/@location,61)"/><br/>
				</a> 
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template name="formatFilename">
		<xsl:variable name="location"><xsl:value-of select="@filename"/></xsl:variable>  
		<xsl:choose>
			<xsl:when test="string-length(@filename)=0">
				<br/>
			</xsl:when>
			<xsl:when test="string-length(@filename)&lt;80">
				<a href="{$location}"><xsl:value-of select="@filename"/></a>
			</xsl:when>
			<xsl:when test="string-length(@filename)&gt;240">
				<a href="{$location}">
					<xsl:value-of select="substring(@filename,1,80)"/><br/>
					<xsl:value-of select="substring(@filename,81,80)"/><br/>
					<xsl:value-of select="substring(@filename,161,80)"/><br/>
					<xsl:value-of select="substring(@filename,241)"/> 
				</a>
			</xsl:when>
			<xsl:when test="string-length(@filename)&gt;160">
				<a href="{$location}">
					<xsl:value-of select="substring(@filename,1,80)"/><br/>
					<xsl:value-of select="substring(@filename,81,80)"/><br/>
					<xsl:value-of select="substring(@filename,161)"/><br/>
				</a>
			</xsl:when>
			<xsl:otherwise>
				<a href="{$location}"> 
					<xsl:value-of select="substring(@filename,1,80)"/><br/>
					<xsl:value-of select="substring(@filename,81)"/><br/>
				</a> 
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

						
</xsl:stylesheet>