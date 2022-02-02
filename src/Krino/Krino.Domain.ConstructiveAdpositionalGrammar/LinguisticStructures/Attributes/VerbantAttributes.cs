using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Verbant attributes.
    /// </summary>
    public class VerbantAttributes : EnumGroupBase
    {
        public VerbantAttributes(EnumGroupBase parent) : base(parent)
        {
            Free = new VerbantLexemeAttributes(this);
            Bound = new VerbantNonLexemeAttributes(this);
        }

        public VerbantLexemeAttributes Free { get; }

        public VerbantNonLexemeAttributes Bound { get; }
    }
}
