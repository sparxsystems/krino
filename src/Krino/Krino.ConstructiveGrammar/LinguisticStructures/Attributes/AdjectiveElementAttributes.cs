using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class AdjectiveElementAttributes : EnumGroupBase
    {
        public AdjectiveElementAttributes(EnumGroupBase parent) : base(parent)
        {
            Attributive = new EnumValue(this);
            PostPositive = new EnumValue(this);
            Predicative = new EnumValue(this);
        }

        /// <summary>
        /// Adjective that comes before the noun (e.g. green book).
        /// </summary>
        public EnumValue Attributive { get; }

        /// <summary>
        /// Adjective that comes after the noun. (e.g. the shortest route possible)
        /// </summary>
        public EnumValue PostPositive { get; }

        /// <summary>
        /// Adjective that comes after the verb (e.g. I was happy).
        /// </summary>
        public EnumValue Predicative { get; }
    }
}
