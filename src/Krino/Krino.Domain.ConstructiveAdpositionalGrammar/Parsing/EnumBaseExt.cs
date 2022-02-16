using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    internal static class EnumBaseExt
    {
        public static string GetGrammarId(this BigInteger attributes)
        {
            var toReplace = string.Join("", nameof(GrammarAttributes), ".");
            var result = GrammarAttributes.Instance.GetFullName(attributes).Replace(toReplace, "");
            return result;
        }

        public static string GetGrammarId(this EnumBase source) => ((BigInteger)source).GetGrammarId();
    }
}
