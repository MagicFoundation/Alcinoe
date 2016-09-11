package com.embarcadero.firemonkey.text;

/* compiled from: FMXTextEditorProxy */
class TextLineSpan {
    private int lineNumber;

    public TextLineSpan(int line) {
        this.lineNumber = line;
    }

    public int getLineNumber() {
        return this.lineNumber;
    }

    public void setLineNumber(int line) {
        this.lineNumber = line;
    }
}
