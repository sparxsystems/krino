using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    public class ActiveVerbAttributes : EnumGroupBase
    {
        public ActiveVerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Transitive = new EnumValue(this);
            Intransitive = new EnumValue(this);
        }

        /// <summary>
        /// Transitive verb.
        /// </summary>
        /// <remarks>
        /// A transitive verb always has a noun that receives the action of the verb, called the direct object.
        /// Transitive verbs sometimes have indirect objects, which name the object to whom or for whom the action was done.
        /// </remarks>
        public EnumValue Transitive { get; }

        /// <summary>
        /// Intransitive verb.
        /// </summary>
        /// <remarks>
        /// An intransitive verb never has a direct or indirect object.
        /// </remarks>
        public EnumValue Intransitive { get; }
    }
}
