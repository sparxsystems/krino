using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Noun semantic attributes.
    /// </summary>
    public class NounSememeAttributes : EnumGroupBase
    {
        public NounSememeAttributes(EnumGroupBase parent) : base(parent)
        {
            Gender = new GenderAttributes(this);
            Number = new NumberAttributes(this);
            Case = new CaseAttributes(this);
        }

        public GenderAttributes Gender { get; }

        public NumberAttributes Number { get; }

        public CaseAttributes Case { get; }
    }
}
