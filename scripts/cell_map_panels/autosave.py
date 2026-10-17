# ChimeraX: name the current view "mine" and save the session as panel.cxs in
# the working directory, every 20 seconds, so that nothing has to be typed.
# Opened from the panel command file; the timer lives as long as the session.
import os
from Qt.QtCore import QTimer
from chimerax.core.commands import run

path = os.path.join(os.getcwd(), "panel.cxs")

def autosave():
    try:
        run(session, "view name mine", log=False)
        run(session, "save %s" % path, log=False)
    except Exception as e:
        session.logger.info("autosave failed: %s" % e)

timer = QTimer()
timer.timeout.connect(autosave)
timer.start(20000)
session._panel_autosave_timer = timer
session.logger.info("The session saves itself to %s every 20 s" % path)
