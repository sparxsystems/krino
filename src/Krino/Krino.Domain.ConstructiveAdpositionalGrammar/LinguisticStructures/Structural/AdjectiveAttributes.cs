using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class AdjectiveAttributes : EnumGroupBase
    {
        public AdjectiveAttributes(EnumGroupBase parent) : base(parent)
        {
            Attributive = new EnumValue(this);
            Predicative = new EnumValue(this);
        }

        /// <summary>
        /// Adjective located before a noun element.
        /// </summary>
        public EnumValue Attributive { get; }

        /// <summary>
        /// Adjective located after a noun element.
        /// </summary>
        public EnumValue Predicative { get; }
    }
}
