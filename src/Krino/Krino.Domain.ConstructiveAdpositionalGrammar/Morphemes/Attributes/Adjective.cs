using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes
{
    /// <summary>
    /// Adjective attributes.
    /// </summary>
    public class Adjective : EnumGroupBase
    {
        public Adjective(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 10)
        {
            Positive = new EnumValue(this, 1);
            Comparative = new EnumValue(this, 2);
            Superlative = new EnumValue(this, 3);
            Attributive = new EnumValue(this, 4);
            Predicative = new EnumValue(this, 5);
            Postpositive = new EnumValue(this, 6);
            Gradable = new EnumValue(this, 7);
            NonGradable = new EnumValue(this, 8);
            Qualitative = new EnumValue(this, 9);
            Classifying = new EnumValue(this, 10);
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
        /// Adjective that comes before the noun (e.g. green book).
        /// </summary>
        public EnumValue Attributive { get; }

        /// <summary>
        /// Adjective that comes after the verb (e.g. I was happy).
        /// </summary>
        public EnumValue Predicative { get; }

        /// <summary>
        /// Adjective that comes after the noun.
        /// </summary>
        public EnumValue Postpositive { get; }
        
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
