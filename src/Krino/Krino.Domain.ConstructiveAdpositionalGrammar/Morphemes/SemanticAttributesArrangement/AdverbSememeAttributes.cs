using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Semantic attributes for adverbs.
    /// </summary>
    public class AdverbSememeAttributes : EnumGroupBase
    {
        public AdverbSememeAttributes(EnumGroupBase parent) : base(parent)
        {
            OfFrequency = new EnumValue(this);
            OfManner = new EnumValue(this);
            OfPlace = new EnumValue(this);
            OfTime = new EnumValue(this);
            OfDegree = new EnumValue(this);
        }

        /// <summary>
        /// How often it happens.
        /// </summary>
        public EnumValue OfFrequency { get; }

        /// <summary>
        /// How it happens.
        /// </summary>
        public EnumValue OfManner { get; }

        /// <summary>
        /// Where it happens.
        /// </summary>
        public EnumValue OfPlace { get; }

        /// <summary>
        /// When it happens.
        /// </summary>
        public EnumValue OfTime { get; }

        /// <summary>
        /// Degree or intensity.
        /// </summary>
        public EnumValue OfDegree { get; }
    }
}
