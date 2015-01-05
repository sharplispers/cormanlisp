// $Header: /usr/local/cvsrep/rdnzl_lisp/examples/AproposGui.cs,v 1.6 2006/01/31 15:17:00 edi Exp $

// Copyright (c) 2004-2006, Dr. Edmund Weitz.  All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:

//   * Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.

//   * Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials
//     provided with the distribution.

// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// compile this with:
//   csc.exe /target:library AproposGui.cs
// and put the resulting DLL into your Lisp's application folder

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

namespace AproposGUI {
  public class AproposControl : System.Windows.Forms.UserControl {
    public System.Windows.Forms.TextBox textBox;
    public System.Windows.Forms.TextBox listBox;
    private System.Windows.Forms.Label label;
    public System.Windows.Forms.Label title;
    private delegate string callback(string input);

    private System.ComponentModel.Container components = null;
    
    public AproposControl() {
      InitializeComponent();
    }

    protected override void Dispose(bool disposing) {
      if (disposing) {
        if (components != null)
          components.Dispose();
      }
      base.Dispose(disposing);
    }

    private void InitializeComponent() {
      this.textBox = new System.Windows.Forms.TextBox();
      this.listBox = new System.Windows.Forms.TextBox();
      this.label = new System.Windows.Forms.Label();
      this.title = new System.Windows.Forms.Label();
      this.SuspendLayout();

      this.textBox.Location = new System.Drawing.Point(16, 344);
      this.textBox.Name = "textBox";
      this.textBox.Size = new System.Drawing.Size(584, 20);
      this.textBox.TabIndex = 0;
      this.textBox.Text = "";

      this.listBox.Location = new System.Drawing.Point(16, 56);
      this.listBox.Multiline = true;
      this.listBox.Name = "listBox";
      this.listBox.ReadOnly = true;
      this.listBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
      this.listBox.Size = new System.Drawing.Size(584, 248);
      this.listBox.TabIndex = 1;
      this.listBox.Text = "";

      this.label.Location = new System.Drawing.Point(24, 312);
      this.label.Name = "label";
      this.label.Size = new System.Drawing.Size(576, 23);
      this.label.TabIndex = 2;
      this.label.Text = "Enter text below and press RETURN";
      this.label.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;

      this.title.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
      this.title.Location = new System.Drawing.Point(24, 16);
      this.title.Name = "title";
      this.title.Size = new System.Drawing.Size(568, 24);
      this.title.TabIndex = 3;
      this.title.Text = "RDNZL Apropos Demo";
      this.title.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;

      this.Controls.Add(this.title);
      this.Controls.Add(this.label);
      this.Controls.Add(this.listBox);
      this.Controls.Add(this.textBox);
      this.Name = "MainControl";
      this.Size = new System.Drawing.Size(616, 384);
      this.ResumeLayout(false);
    }
  }
}
