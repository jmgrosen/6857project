class TopoHiding(object):
    def __init__(self, public_keys, bit):
        self.public_keys = public_keys
        self.bit = bit

    def do_round(self, i, msgs):
        """Given a round number and the messages from the other nodes, return the list
        of messages to send out to the connected nodes.
        """
        return ["hello world"]*self.public_keys
