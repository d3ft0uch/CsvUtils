package extensions;

import java.io.*;

public interface OutputDispatcher {

    PrintStream openStreamForSheet(String sheetName) throws FileNotFoundException;

    void closeStreamForSheet(PrintStream stream, String sheetName);
}