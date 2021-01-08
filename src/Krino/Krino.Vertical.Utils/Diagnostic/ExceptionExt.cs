using System;
using System.Text;

namespace Krino.Vertical.Utils.Diagnostic
{
    internal static class ExceptionExt
    {
        public static string GetDetailsFromException(this Exception err)
        {
            // If there is not exception, then return empty string.
            if (err == null)
            {
                return "";
            }

            try
            {
                // Get the exception details.
                StringBuilder aDetails = new StringBuilder();
                aDetails.AppendLine("Exception:").Append(err.GetType()).Append(": ").AppendLine(err.Message).Append(err.StackTrace);

                // Get all inner exceptions.
                Exception anInnerException = err.InnerException;
                while (anInnerException != null)
                {
                    aDetails.AppendLine().AppendLine();
                    aDetails.Append(anInnerException.GetType()).Append(": ").AppendLine(anInnerException.Message).Append(anInnerException.StackTrace);

                    // Get the next inner exception.
                    anInnerException = anInnerException.InnerException;
                }

                aDetails.Append("\r\n==========\r\n");

                return aDetails.ToString();
            }
            catch (Exception e)
            {
                return "Exception: Trace failed to retrieve excepion details. Message: " + e.Message;
            }
        }
    }
}
