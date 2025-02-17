with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

package body OpenSSL is
   procedure Add_All_Algorithms is
   begin
      Init_Crypto (Init_Add_All_Ciphers or Init_Add_All_Digests, null);
   end Add_All_Algorithms;

   procedure Load_Error_Strings is
   begin
      Init_SSL (Init_Load_Crypto_Strings or Init_Load_SSL_Strings, null);
   end Load_Error_Strings;

   procedure Log_Errors is
      E : Natural;
   begin
      loop
         E := Get_Error;
         exit when E = 0;
         Put_Line (Standard_Error, "Error: " & Value (Error_String (E, Null_Ptr)));
      end loop;
      Set_Exit_Status (Failure);
   end Log_Errors;
end OpenSSL;
