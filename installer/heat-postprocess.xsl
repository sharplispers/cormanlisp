<?xml version="1.0" encoding="UTF-8"?>
<!--
Created by Artem Boldariev <artem.boldarev@gmail.com>, 2018.
This file is distributed under the terms of CC0 license (Public Domain).

See the 'LICENSE.txt' file for the additional details.
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:wix="http://schemas.microsoft.com/wix/2006/wi">
  <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

  <!-- Include  -->
  <!--
      http://windows-installer-xml-wix-toolset.687559.n2.nabble.com/Transform-output-of-heat-to-insert-an-include-statement-td7355923.html
  -->
  <xsl:template match="wix:Wix">
    <xsl:copy>
      <xsl:processing-instruction
          name="include">Config.wxi</xsl:processing-instruction>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <!-- Add Win64 attribute to the all components -->
  <xsl:template match="wix:Component">
    <xsl:copy>
      <xsl:apply-templates select="@*" />
      <xsl:attribute name="Win64">$(var.Win64)</xsl:attribute>

      <!-- Now take the rest of the inner tag -->
      <xsl:apply-templates select="node()" />
    </xsl:copy>
  </xsl:template>

  <!--
      Alter File Table sequencing to improve performance of un/installation.
      Info:
      http://windows-installer-xml-wix-toolset.687559.n2.nabble.com/Performance-Issues-with-File-table-sequencing-td4777168.html
      https://stackoverflow.com/questions/15967087/how-can-i-sort-file-sequence-in-wix?rq=1
  -->
  <xsl:template match="wix:File">
    <xsl:variable name="parentDirName">
      <xsl:value-of select="/wix:Wix/wix:Fragment/wix:DirectoryRef/wix:Directory/@Name"/>
    </xsl:variable>
    <xsl:variable name="alphaNumeric">
      <xsl:value-of select="'_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="Id">
        <xsl:variable name="fileName">
          <xsl:value-of select="concat($parentDirName, substring-after(@Source, '\'))"/>
        </xsl:variable>
        <!-- remove all non alphanumeric characters, make a unique string -->
        <xsl:value-of select="concat(substring(translate($fileName, translate($fileName, $alphaNumeric, ''), ''), 1, 72 - string-length(@Id)), @Id)"/>
      </xsl:attribute>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="wix:Directory">
    <xsl:variable name="parentDirName">
      <xsl:value-of select="/wix:Wix/wix:Fragment/wix:DirectoryRef/wix:Directory/@Name"/>
    </xsl:variable>
    <xsl:variable name="parentPath">
      <xsl:for-each select="ancestor::wix:Directory/@Name">
        <xsl:value-of select="concat(.,'_')"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="alphaNumeric">
      <xsl:value-of select="'_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="Id">
        <xsl:variable name="fileName">
          <xsl:value-of select="concat($parentDirName, concat($parentPath, @Name))"/>
        </xsl:variable>
        <!-- remove all non alphanumeric characters, make a unique string -->
        <xsl:value-of select="concat(substring(translate($fileName, translate($fileName, $alphaNumeric, ''), ''), 1, 72 - string-length(@Id)), @Id)"/>
      </xsl:attribute>
      <xsl:apply-templates select="*"/>
    </xsl:copy>
  </xsl:template>

  <!-- Identity transform. -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>

