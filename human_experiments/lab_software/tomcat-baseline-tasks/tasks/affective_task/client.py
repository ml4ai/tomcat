import pygame
from common import (cursor_visibility, render_blank_screen,
                    set_cursor_position, wait)
from config import CLIENT_WINDOW_HEIGHT, CLIENT_WINDOW_WIDTH
from network import receive, send

from .config import (BLANK_SCREEN_MILLISECONDS,
                    CROSS_SCREEN_MILLISECONDS,
                    DISPLAY_AFFEC_DISCUSSION_MILLISECONDS)

from .utils import (Button, render_image_center,
                    submit_button, timer,
                    display_msg_affective_disscussion)

from common import render_text_center


class ClientAffectiveTask:
    def __init__(self, from_server, to_server, screen):
        self._from_server = from_server
        self._to_server = to_server
        self._screen = screen

    def run(self, collaboration: bool = False):
        valence_buttons = []
        valence_buttons.append(Button((-345, 220), self._screen))
        valence_buttons.append(Button((-175, 220), self._screen))
        valence_buttons.append(Button((-2, 220), self._screen))
        valence_buttons.append(Button((173, 220), self._screen))
        valence_buttons.append(Button((343, 220), self._screen))

        arousal_buttons = []
        arousal_buttons.append(Button((-345, -130), self._screen))
        arousal_buttons.append(Button((-175, -130), self._screen))
        arousal_buttons.append(Button((-2, -130), self._screen))
        arousal_buttons.append(Button((173, -130), self._screen))
        arousal_buttons.append(Button((343, -130), self._screen))

        print("[STATUS] Running affective task")

        while True:

            [data] = receive([self._from_server])

            if data["type"] == "request":
                if data["request"] == "end":
                    break
            elif data["type"] == "state":
                state = data["state"]
            else:
                # Read the next message
                continue

            # show a blank screen and a cross before showing an image
            render_blank_screen(self._screen, BLANK_SCREEN_MILLISECONDS)

            render_image_center(
                "./tasks/affective_task/images/plus.png", self._screen, refresh=True)
            wait(CROSS_SCREEN_MILLISECONDS)

            if collaboration:
                # displaying a slide asking subjects not to discuss
                render_blank_screen(self._screen, BLANK_SCREEN_MILLISECONDS)
                display_msg_affective_disscussion(
                    self._screen, "Observe", DISPLAY_AFFEC_DISCUSSION_MILLISECONDS/2)
                render_blank_screen(self._screen, BLANK_SCREEN_MILLISECONDS)

                # show an image for team task for the team to analyze seperately
                render_image_center(
                    state["image_path"], self._screen, refresh=True)
                render_text_center(
                    "Quiet", (950, 50), self._screen, font_size=45, x_offset=0, y_offset=450)
                timer(state["image_timer"], [
                ], "Team: " if collaboration else "Individual: ", self._screen)

                # displaying a slide asking subjects to discuss
                render_blank_screen(self._screen, BLANK_SCREEN_MILLISECONDS)
                display_msg_affective_disscussion(
                    self._screen, "Discuss", DISPLAY_AFFEC_DISCUSSION_MILLISECONDS/2)
                render_blank_screen(self._screen, BLANK_SCREEN_MILLISECONDS)

                # show the same image again for team task for the team to dicuss their findings
                render_image_center(
                    state["image_path"], self._screen, refresh=True)
                render_text_center(
                    "Discuss", (950, 50), self._screen, font_size=45, x_offset=0, y_offset=450)
                timer(state["discussion_timer"], [], "Team: ", self._screen)

            else:
                # show an image for individual task
                render_image_center(
                    state["image_path"], self._screen, refresh=True)
                # show timer above image until timer runs out
                timer(state["image_timer"], [
                ], "Team: " if collaboration else "Individual: ", self._screen)

            if collaboration:
                if state["selected"]:
                    # slide before that shows up based on the client that is selected before the buttons are displayed
                    render_blank_screen(
                        self._screen, BLANK_SCREEN_MILLISECONDS)
                    display_msg_affective_disscussion(
                        self._screen, "You have been selected for rating the images", DISPLAY_AFFEC_DISCUSSION_MILLISECONDS)
                    render_blank_screen(
                        self._screen, BLANK_SCREEN_MILLISECONDS)
                else:
                    render_blank_screen(
                        self._screen, 2 * BLANK_SCREEN_MILLISECONDS + DISPLAY_AFFEC_DISCUSSION_MILLISECONDS)

            # show valence and arousal scoring
            render_image_center("./tasks/affective_task/images/buttons_images/Valence.jpg",
                                self._screen,
                                y_offset=-150,
                                refresh=True)
            render_image_center("./tasks/affective_task/images/buttons_images/Arousal.jpg",
                                self._screen,
                                y_offset=200)

            render_text_center("Valence score", (400, 50),
                               self._screen, y_offset=-270)
            render_text_center("Upset", (250, 50), self._screen,
                               font_size=30, x_offset=-530, y_offset=-120)
            render_text_center("Happy", (250, 50), self._screen,
                               font_size=30, x_offset=530, y_offset=-120)

            render_text_center("-2", (300, 50), self._screen,
                               font_size=25, x_offset=-340, y_offset=-55)
            render_text_center("-1", (300, 50), self._screen,
                               font_size=25, x_offset=-165, y_offset=-55)
            render_text_center("0", (300, 50), self._screen,
                               font_size=25, x_offset=0, y_offset=-55)
            render_text_center("+1", (300, 50), self._screen,
                               font_size=25, x_offset=165, y_offset=-55)
            render_text_center("+2", (300, 50), self._screen,
                               font_size=25, x_offset=335, y_offset=-55)

            render_text_center("Arousal score", (400, 50),
                               self._screen, y_offset=80)
            render_text_center("Calm", (300, 50), self._screen,
                               font_size=30, x_offset=-540, y_offset=220)
            render_text_center("Excitation", (300, 50), self._screen,
                               font_size=30, x_offset=530, y_offset=220)

            render_text_center("-2", (300, 50), self._screen,
                               font_size=25, x_offset=-340, y_offset=290)
            render_text_center("-1", (300, 50), self._screen,
                               font_size=25, x_offset=-165, y_offset=290)
            render_text_center("0", (300, 50), self._screen,
                               font_size=25, x_offset=0, y_offset=290)
            render_text_center("+1", (300, 50), self._screen,
                               font_size=25, x_offset=165, y_offset=290)
            render_text_center("+2", (300, 50), self._screen,
                               font_size=25, x_offset=335, y_offset=290)

            remove_button_frame = not state["selected"]

            for button in arousal_buttons:
                button.unselect(remove_button_frame)

            for button in valence_buttons:
                button.unselect(remove_button_frame)

            # center cursor
            set_cursor_position(CLIENT_WINDOW_WIDTH / 2,
                                CLIENT_WINDOW_HEIGHT / 2)

            if state["selected"]:
                cursor_visibility(True)

            if state["selected"]:
                submit = submit_button(self._screen, y_offset_from_center=400)

            # render button response while timer is running
            def button_response(events) -> bool:
                # participant must select the ratings
                if state["selected"]:
                    for event in events:
                        if event.type == pygame.MOUSEBUTTONDOWN:
                            # submit button is selected
                            if state["selected"] and submit.collidepoint(pygame.mouse.get_pos()):
                                # ensure that arousal rating is selected
                                arousal_rating_selected = False
                                for button in arousal_buttons:
                                    if button.is_selected():
                                        arousal_rating_selected = True
                                        break

                                # ensure that valence rating is selected
                                valence_rating_selected = False
                                for button in valence_buttons:
                                    if button.is_selected():
                                        valence_rating_selected = True
                                        break

                                # end the current rating period
                                if arousal_rating_selected and valence_rating_selected:
                                    update = {"type": "update_end"}
                                    send([self._to_server], update)

                                    # return True to end the timer
                                    return True

                            # if any of the arousal buttons is selected
                            for i, button in enumerate(arousal_buttons):
                                if button.object.collidepoint(pygame.mouse.get_pos()):
                                    button.select()
                                    for j, each_button in enumerate(arousal_buttons):
                                        if j != i:
                                            each_button.unselect()

                                    update = {
                                        "type": "update",
                                        "update": {
                                            "rating_type": "arousal",
                                            "rating_index": i
                                        }
                                    }
                                    send([self._to_server], update)

                                    break

                            # if any of the valence buttons is selected
                            else:
                                for i, button in enumerate(valence_buttons):
                                    if button.object.collidepoint(pygame.mouse.get_pos()):
                                        button.select()
                                        for j, each_button in enumerate(valence_buttons):
                                            if j != i:
                                                each_button.unselect()

                                        update = {
                                            "type": "update",
                                            "update": {
                                                "rating_type": "valence",
                                                "rating_index": i
                                            }
                                        }
                                        send([self._to_server], update)

                                        break
                    return False

                # participants do not select the rating
                else:
                    data = receive([self._from_server], 0.0)
                    if data:
                        data = data[0]
                        if data["type"] == "update":
                            update = data["update"]
                            if update["rating_type"] == "arousal":
                                arousal_buttons[update["rating_index"]].select(
                                )
                                for j, button in enumerate(arousal_buttons):
                                    if j != update["rating_index"]:
                                        button.unselect(no_frame=True)
                            else:
                                valence_buttons[update["rating_index"]].select(
                                )
                                for j, button in enumerate(valence_buttons):
                                    if j != update["rating_index"]:
                                        button.unselect(no_frame=True)
                        elif data["type"] == "update_end":
                            return True
                        else:
                            raise RuntimeError(
                                "Cannot handle message type: " + data["type"])

                    return False

            timer(state["rating_timer"], [button_response],
                  "Rating ends in: ", self._screen, display_timer=2)

            if state["selected"]:
                cursor_visibility(False)

                # send valence and arousal data to server
                arousal = None
                for i, button in enumerate(arousal_buttons):
                    if button.is_selected():
                        arousal = i - 2
                        break

                valence = None
                for i, button in enumerate(valence_buttons):
                    if button.is_selected():
                        valence = i - 2
                        break

                response = {
                    "type": "rating",
                    "rating": {
                        "arousal": arousal,
                        "valence": valence
                    }
                }

                send([self._to_server], response)

        print("[STATUS] Affective task ended")
