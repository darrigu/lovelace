with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package OpenSSL is
   type Init_Settings_Type is null record;

   Init_Add_All_Ciphers : constant unsigned_long_long := 16#00000004#;
   Init_Add_All_Digests : constant unsigned_long_long := 16#00000008#;

   procedure Init_Crypto (Opts : unsigned_long_long; Settings : access Init_Settings_Type);
   pragma Import (C, Init_Crypto, "OPENSSL_init_crypto");

   procedure Add_All_Algorithms;

   Init_Load_Crypto_Strings : constant unsigned_long_long := 16#00000002#;
   Init_Load_SSL_Strings    : constant unsigned_long_long := 16#00200000#;

   procedure Init_SSL (Opts : unsigned_long_long; Settings : access Init_Settings_Type);
   pragma Import (C, Init_SSL, "OPENSSL_init_ssl");

   procedure Load_Error_Strings;

   type SSL_Method_Type is null record;

   function TLS_Client_Method return access SSL_Method_Type;
   pragma Import (C, TLS_Client_Method, "TLS_client_method");

   type SSL_Ctx_Type is null record;

   function SSL_Ctx_New (Meth : access SSL_Method_Type) return access SSL_Ctx_Type;
   pragma Import (C, SSL_Ctx_New, "SSL_CTX_new");

   procedure SSL_Ctx_Free (Ctx : access SSL_Ctx_Type);
   pragma Import (C, SSL_Ctx_Free, "SSL_CTX_free");

   type SSL_Type is null record;

   function SSL_New (Ctx : access SSL_Ctx_Type) return access SSL_Type;
   pragma Import (C, SSL_New, "SSL_new");

   procedure SSL_Free (SSL : access SSL_Type);
   pragma Import (C, SSL_Free, "SSL_free");

   function SSL_Set_Fd (SSL : access SSL_Type; Fd : Integer) return Integer;
   pragma Import (C, SSL_Set_Fd, "SSL_set_fd");

   SSL_Sent_Shutdown     : constant unsigned_long_long := 1;
   SSL_Received_Shutdown : constant unsigned_long_long := 2;

   procedure SSL_Set_Shutdown (SSL : access SSL_Type; Mode : unsigned_long_long);
   pragma Import (C, SSL_Set_Shutdown, "SSL_set_shutdown");

   procedure SSL_Shutdown (SSL : access SSL_Type);
   pragma Import (C, SSL_Shutdown, "SSL_shutdown");

   function SSL_Connect (SSL : access SSL_Type) return Integer;
   pragma Import (C, SSL_Connect, "SSL_connect");

   function SSL_Read (SSL : access SSL_Type; Buf : chars_ptr; Num : Natural) return Integer;
   pragma Import (C, SSL_Read, "SSL_read");

   function SSL_Write (SSL : access SSL_Type; Buf : chars_ptr; Num : Natural) return Integer;
   pragma Import (C, SSL_Write, "SSL_write");

   function Get_Error return Natural;
   pragma Import (C, Get_Error, "ERR_get_error");

   function Error_String (E : Natural; Buf : chars_ptr) return chars_ptr;
   pragma Import (C, Error_String, "ERR_error_string");

   procedure Log_Errors;
end OpenSSL;
