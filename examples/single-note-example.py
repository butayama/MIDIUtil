############################################################################
# A sample program to create a single-track MIDI file, add a note,
# and write to disk.
############################################################################

# Import the library
from midiutil import MIDIFile

# Create the MIDIFile Object
MyMIDI = MIDIFile(1)

# Add track name and tempo. The first argument to add_track_name and
# add_tempo is the time to write the event.
track = 0
time = 0
MyMIDI.add_track_name(track, time, "Sample Track")
MyMIDI.add_tempo(track, time, 120)

# Add a note. add_note expects the following information:
channel = 0
pitch = 60
duration = 1
volume = 100

# Now add the note.
MyMIDI.add_note(track, channel, pitch, time, duration, volume)

# And write it to disk.
with open("output.mid", 'wb') as binfile:
    MyMIDI.write_file(binfile)
