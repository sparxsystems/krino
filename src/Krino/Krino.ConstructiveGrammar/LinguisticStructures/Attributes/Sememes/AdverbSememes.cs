using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Semantic attributes for adverbs.
    /// </summary>
    public class AdverbSememes : EnumGroupBase
    {
        public AdverbSememes(EnumGroupBase parent) : base(parent)
        {
            OfFrequency = new EnumValue(this);
            OfManner = new EnumValue(this);
            OfPlace = new EnumValue(this);
            OfTime = new EnumValue(this);
            OfDegree = new AdverbDegreeSememes(this);
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
        /// <remarks>
        /// https://studfile.net/preview/4083865/page:23/
        /// </remarks>
        public AdverbDegreeSememes OfDegree { get; }
    }
}
