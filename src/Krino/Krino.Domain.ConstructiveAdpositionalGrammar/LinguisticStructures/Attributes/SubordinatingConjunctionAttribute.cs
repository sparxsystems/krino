using Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class SubordinatingConjunctionAttribute : EnumGroupBase
    {
        public SubordinatingConjunctionAttribute(EnumGroupBase parent) : base(parent)
        {
            Sememe = new SubordinatingConjunctionSememes(this);
        }

        public SubordinatingConjunctionSememes Sememe { get; }
    }
}
