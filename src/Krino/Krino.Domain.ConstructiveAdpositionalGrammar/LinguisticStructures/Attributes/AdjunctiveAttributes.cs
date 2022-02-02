using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Adjunctive attributes.
    /// </summary>
    public class AdjunctiveAttributes : EnumGroupBase
    {
        public AdjunctiveAttributes(EnumGroupBase parent) : base(parent)
        {
            Free = new AdjunctiveLexemeAttributes(this);
            Bound = new AdjunctiveNonLexemeAttributes(this);
        }

        public AdjunctiveLexemeAttributes Free { get; }

        public AdjunctiveNonLexemeAttributes Bound { get; }
    }
}
