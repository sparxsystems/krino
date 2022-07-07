using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Adjective attributes.
    /// </summary>
    public class AdjectiveAttributes : EnumGroupBase
    {
        public AdjectiveAttributes(EnumGroupBase parent) : base(parent)
        {
            Positive = new EnumValue(this);
            Comparative = new EnumValue(this);
            Superlative = new EnumValue(this);
            Gradable = new EnumValue(this);
            NonGradable = new EnumValue(this);
            Qualitative = new EnumValue(this);
            Classifying = new EnumValue(this);
        }

        /// <summary>
        /// Default/basic form of the adjective.
        /// </summary>
        public EnumValue Positive { get; }

        /// <summary>
        /// Comparative form to compare two people or things e.g. happier.
        /// </summary>
        public EnumValue Comparative { get; }

        /// <summary>
        /// Superlative form to compare one person ot thing with everybody or anything in the group.
        /// </summary>
        public EnumValue Superlative { get; }

        /// <summary>
        /// Can be modified by placing one or more adverbs in front of them e.g. very expensive car. (very is an adverb)
        /// </summary>
        public EnumValue Gradable { get; }

        /// <summary>
        /// Cannot be modified by adverbs. e.g. electronic devices
        /// </summary>
        public EnumValue NonGradable { get; }

        /// <summary>
        /// Describes qualities of a person or thing. e.g. tall man.
        /// </summary>
        public EnumValue Qualitative { get; }

        /// <summary>
        /// Places people into categories or classes. e.g. nuclear weapon.
        /// </summary>
        public EnumValue Classifying { get; }

    }
}
