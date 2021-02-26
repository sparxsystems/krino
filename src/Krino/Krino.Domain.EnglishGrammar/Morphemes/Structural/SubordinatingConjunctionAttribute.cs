using Krino.Domain.EnglishGrammar.Morphemes.Semantic;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
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
