using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement
{
    /// <summary>
    /// Adjunctive attributes.
    /// </summary>
    public class AdjunctiveAttributes : EnumGroupBase
    {
        public AdjunctiveAttributes(EnumGroupBase parent) : base(parent)
        {
            Adjective = new AdjectiveAttributes(this);
            Determiner = new DeterminerAttributes(this);
            Numeral = new NumeralAttributes(this);
        }

        /// <summary>
        /// Adjunctive is an adjenctive.
        /// </summary>
        public AdjectiveAttributes Adjective { get; }

        /// <summary>
        /// Adjunctive is a determiner.
        /// </summary>
        public DeterminerAttributes Determiner { get; }

        /// <summary>
        /// Adjunctive is a numeral.
        /// </summary>
        public NumeralAttributes Numeral { get; }
    }
}
