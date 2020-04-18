using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes
{
    /// <summary>
    /// Conjunction attributes.
    /// </summary>
    public class Conjunction : EnumGroupBase
    {
        public Conjunction(EnumGroupBase parent, int localPosition) : base(parent, 3, localPosition)
        {
            Coordinating = new EnumValue(this, 1);
            Subordinating = new EnumValue(this, 2);
            Starting = new EnumValue(this, 3);
        }

        /// <summary>
        /// Connects equal phrases e.g. and, or, but.
        /// </summary>
        public EnumValue Coordinating { get; }

        /// <summary>
        /// Connects subordinated phrase to the main phrase e.g. until, because.
        /// </summary>
        public EnumValue Subordinating { get; }

        /// <summary>
        /// Conjuction which can be at the begnnig of the sentence e.g. And, But.
        /// </summary>
        public EnumValue Starting { get; }
    }
}
