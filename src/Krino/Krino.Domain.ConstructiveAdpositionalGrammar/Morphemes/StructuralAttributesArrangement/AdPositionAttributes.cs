using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement
{
    /// <summary>
    /// AdPosition attributes.
    /// </summary>
    public class AdPositionAttributes : EnumGroupBase
    {
        public AdPositionAttributes(EnumGroupBase parent) : base(parent)
        {
            Preposition = new EnumValue(this);
            Postposition = new EnumValue(this);
            Conjunction = new ConjunctionAttributes(this);
        }

        /// <summary>
        /// AdPosition is a preposition.
        /// </summary>
        public EnumValue Preposition { get; }

        /// <summary>
        /// AdPosition is a postposition.
        /// </summary>
        public EnumValue Postposition { get; }

        /// <summary>
        /// AdPosition is a conjunction.
        /// </summary>
        public ConjunctionAttributes Conjunction { get; }
    }
}
