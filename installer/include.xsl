<?xml version="1.0" encoding="UTF-8"?>
<!--
http://windows-installer-xml-wix-toolset.687559.n2.nabble.com/Transform-output-of-heat-to-insert-an-include-statement-td7355923.html
-->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:wix="http://schemas.microsoft.com/wix/2006/wi">
  <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

  <xsl:template match="wix:Wix">
    <xsl:copy>
      <xsl:processing-instruction
          name="include">Config.wxi</xsl:processing-instruction>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <!-- Identity transform. -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
