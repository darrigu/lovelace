with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with OpenSSL; use OpenSSL;
with Ada.Command_Line; use Ada.Command_Line;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   Exit_Program : exception;

   Gemini_Prefix : constant String := "gemini://";
   Gemini_Port   : constant Port_Type := 1965;

   Config           : Command_Line_Configuration;
   End_Of_Arguments : Boolean;

   URL  : constant String := Get_Argument (End_Of_Arguments => End_Of_Arguments);
   Host : Unbounded_String;

   Server : Sock_Addr_Type;
   Client : Socket_Type;

   Ctx : access SSL_Ctx_Type;
   SSL : access SSL_Type;
begin
   Getopt (Config);

   if End_Of_Arguments then
      Display_Help (Config);
      Put_Line (Standard_Error, "Error: Expected Gemini URL");
      Set_Exit_Status (Failure);
      raise Exit_From_Command_Line;
   end if;

   Host := To_Unbounded_String (URL);

   if Index (Host, Gemini_Prefix) = 0 then
      Put_Line (Standard_Error, "Error: Invalid URL prefix (expected " & Gemini_Prefix & ")");
      Set_Exit_Status (Failure);
      raise Exit_From_Command_Line;
   end if;

   Delete (Host, 1, Gemini_Prefix'Length);

   declare
      Slash_Pos : constant Natural := Index (Host, "/");
   begin
      if Slash_Pos > 0 then
         Delete (Host, Slash_Pos, Length (Host));
      end if;
   end;

   Server.Addr := Addresses (Get_Host_By_Name (To_String (Host)), 1);
   Server.Port := Gemini_Port;
   Create_Socket (Client, Level => IP_Protocol_For_TCP_Level);

   Connect_Socket (Client, Server);

   Add_All_Algorithms;
   Load_Error_Strings;
   Ctx := SSL_Ctx_New (TLS_Client_Method);

   if Ctx = null then
      Put_Line (Standard_Error, "Error: Could not initialize the SSL context");
      Set_Exit_Status (Failure);
      raise Exit_Program;
   end if;

   SSL := SSL_New (Ctx);
   SSL_Set_Fd (SSL, To_C (Client));
   SSL_Set_Shutdown (SSL, SSL_Sent_Shutdown or SSL_Received_Shutdown);

   if SSL_Connect (SSL) < 0 then
      Put_Line (Standard_Error, "Error: Could not connect to server via SSL");
      Set_Exit_Status (Failure);
      raise Exit_Program;
   end if;

   declare
      Request : constant String := URL & CR & LF;

      Response : char_array_access := new char_array (1 .. 1024);
      Read     : Integer;
   begin
      if SSL_Write (SSL, New_String (Request), Request'Length) <= 0 then
         Log_Errors;
         raise Exit_Program;
      end if;

      loop
         Read := SSL_Read(SSL, To_Chars_Ptr (Response), Response'Length);
         exit when Read = 0;

         if Read < 0 then
            Log_Errors;
            raise Exit_Program;
         end if;

         Put (Value (To_Chars_Ptr (Response), size_t (Read)));
      end loop;
   end;

   raise Exit_Program;
exception
   when Exit_Program =>
      SSL_Shutdown (SSL);
   when Exit_From_Command_Line =>
      null;
end;
