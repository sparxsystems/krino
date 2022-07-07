using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Parsing
{
    public static class EnumBaseExt
    {
        private static readonly string myToReplace = string.Join("", nameof(GrammarAttributes), ".");
        private static Dictionary<BigInteger, string> myBuffer = new Dictionary<BigInteger, string>();

        public static string GetGrammarId(this BigInteger attributes)
        {
            using var _t = Trace.Entering();

            string result = "";

            if (attributes != 0 && !myBuffer.TryGetValue(attributes, out result))
            {
                result = GrammarAttributes.Instance.GetFullName(attributes).Substring(myToReplace.Length);
                myBuffer[attributes] = result;
            }

            return result;
        }

        public static string GetGrammarId(this EnumBase source) => ((BigInteger)source).GetGrammarId();
    }
}
