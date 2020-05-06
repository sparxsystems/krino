using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// AdPosition attributes.
    /// </summary>
    public class AdPositionAttributes : EnumGroupBase
    {
        public AdPositionAttributes(EnumGroupBase parent) : base(parent)
        {
            Conjunction = new ConjunctionAttributes(this);
        }

        /// <summary>
        /// AdPosition is a conjunction.
        /// </summary>
        public ConjunctionAttributes Conjunction { get; }
    }
}
