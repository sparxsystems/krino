using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Gender attributes.
    /// </summary>
    public class GenderAttributes : EnumGroupBase
    {
        public GenderAttributes(EnumGroupBase parent) : base(parent)
        {
            Masculine = new EnumValue(this);
            Feminine = new EnumValue(this);
            Neuter = new EnumValue(this);
        }

        public EnumValue Masculine { get; }

        public EnumValue Feminine { get; }

        public EnumValue Neuter { get; }
    }
}
